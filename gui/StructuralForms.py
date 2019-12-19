import wx
import os
import sys
import wx.grid
import numpy as np
import subprocess
import bin.EnhancedStatusBar as ESB
#=======================================
#=====  foldpanelbar  ==================
#=======================================
import matplotlib
matplotlib.use('WXAgg')
from matplotlib.figure import Figure
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigCanvas, \
    NavigationToolbar2WxAgg as NavigationToolbar

from matplotlib.path import Path
import matplotlib.patches as patches

from bin.Dialogs import DimDialog, NodalForce, \
                    ViewOptions, Materials, Sections, Support, EditElement, \
                    F1, F3, F4, F5, Analysis_Options, \
                    Optimization_Options_DE
from bin.Output import OutputFrame

class MainFrame(wx.Frame):
    title = 'StructuralForms'
    def __init__(self):
        wx.Frame.__init__(self, None, -1, self.title, style=wx.CAPTION       |  
                                                          wx.CLOSE_BOX     | 
                                                          wx.MINIMIZE_BOX  | 
                                                          wx.MAXIMIZE_BOX  | 
                                                          wx.RESIZE_BORDER | 
                                                          wx.SYSTEM_MENU   | 
                                                          wx.CLIP_CHILDREN
                                                          )
        
        self.SetBackgroundColour('white')
#      FOR PyInstaller =======================
#        if getattr(sys, 'frozen', False):
#            self.PATH = os.path.dirname(sys.executable)
#        elif __file__:
#            self.PATH = os.path.dirname(__file__)
        self.PATH = os.getcwd()
            
#        print('WWMLK')
#        print(os.getcwd())
#        print('WWMLK')
#        print(self.PATH)
#        print('WWMLK')
        
        self.Lim_X1 = -3.0
        self.Lim_Y1 = -3.0
        self.Lim_X2 = 17.0
        self.Lim_Y2 = 17.0
        
        self.Grid_Spacing = 1

        self.accuracy = 1001

        self.x_tick = np.arange(self.Lim_X1,self.Lim_X2,self.Grid_Spacing)
        self.y_tick = np.arange(self.Lim_Y1,self.Lim_Y2,self.Grid_Spacing)

        self.create_menu()
        self.create_status_bar()
        self.create_main_panel()

#        dlg = DimDialog(self.Lim_X1,self.Lim_Y1,self.Lim_X2,self.Lim_Y2,self.Grid_Spacing)
#        dlg.CenterOnScreen()
#        val = dlg.ShowModal()
#        if val == wx.ID_OK:
#            self.Lim_X1 = dlg.Lim_X1
#            self.Lim_Y1 = dlg.Lim_Y1
#            self.Lim_X2 = dlg.Lim_X2
#            self.Lim_Y2 = dlg.Lim_Y2
#            self.Grid_Spacing = dlg.Grid_spacing
#            
#            self.Dim_X = self.Lim_X2 - self.Lim_X1
#            self.Dim_Y = self.Lim_Y2 - self.Lim_Y1
#            self.draw_figure()
#        else:
#            pass
#            
#        dlg.Destroy() 
        
        self.Dim_X = self.Lim_X2 - self.Lim_X1
        self.Dim_Y = self.Lim_Y2 - self.Lim_Y1
        self.draw_figure()
        
    def create_menu(self):
        self.menubar = wx.MenuBar()
#=============================================================================        
        menu_file = wx.Menu()
        m_new = menu_file.Append(-1, "&New\tCtrl-N", "Create empty file")
        self.Bind(wx.EVT_MENU, self.on_new, m_new)
        m_open = menu_file.Append(-1, "&Open\tCtrl-O", "Open existing file")
        self.Bind(wx.EVT_MENU, self.on_open, m_open)
        m_expt = menu_file.Append(-1, "&Save\tCtrl-S", "Save file")
        self.Bind(wx.EVT_MENU, self.on_save, m_expt)
        menu_file.AppendSeparator()
        m_exit = menu_file.Append(-1, "E&xit\tCtrl-X", "Exit")
        self.Bind(wx.EVT_MENU, self.on_exit, m_exit)
#============================================================================= 
        self.menu_view = wx.Menu()
        m_zoom = self.menu_view.Append(-1, "&Zoom", "Zoom on window")
        self.Bind(wx.EVT_MENU, self.on_zoom, m_zoom)
        m_pan = self.menu_view.Append(10000, "&Pan", "Pan", wx.ITEM_CHECK)
        self.Bind(wx.EVT_MENU, self.on_pan, m_pan)
        m_reset = self.menu_view.Append(-1, "&Reset View", "Reset View")
        self.Bind(wx.EVT_MENU, self.on_reset, m_reset)
        self.menu_view.AppendSeparator()
        self.m_grid = self.menu_view.Append(20000, "&Show Grid", "Show Grid", wx.ITEM_CHECK)
        self.Bind(wx.EVT_MENU, self.on_cb_grid, self.m_grid)
        self.menu_view.Check(20000, True)
        m_dim = self.menu_view.Append(-1, "&Dimensions", "Overall dimensions and grid spacing")
        self.Bind(wx.EVT_MENU, self.OnClickDimensions, m_dim)
        self.menu_view.AppendSeparator()
        m_opt = self.menu_view.Append(-1, "&View Options", "View Options")
        self.Bind(wx.EVT_MENU, self.OnClickViewOptions, m_opt)
#=============================================================================   
        menu_calculate = wx.Menu()
        m_static = menu_calculate.Append(-1, "&Static Analysis", "Static Analysis")
        self.Bind(wx.EVT_MENU, self.on_calculte, m_static)
        menu_calculate.AppendSeparator()
        m_options = menu_calculate.Append(-1, "&Options", "Analysis Options")
        self.Bind(wx.EVT_MENU, self.on_options, m_options)
#=============================================================================       
        menu_help = wx.Menu()
        m_about = menu_help.Append(-1, "&About\tF1", "About the demo")
        self.Bind(wx.EVT_MENU, self.on_about, m_about)
#=============================================================================         
        self.menubar.Append(menu_file, "&File")
        self.menubar.Append(self.menu_view, "&View")
        self.menubar.Append(menu_calculate, "&Calculate")
        self.menubar.Append(menu_help, "&Help")
        self.SetMenuBar(self.menubar)

    def on_zoom(self,event):
        self.vzoom = True
        self.toolbar.zoom()
        
    def on_pan(self,event):
        self.vpan = True
        self.toolbar.pan()
        
    def on_reset(self,Event):
        if self.vpan == True:
            self.toolbar.pan()
            self.menu_view.Check(10000, False)
            self.vpan = False
        self.toolbar.home()
        
    def on_calculte(self,event):
        self.OnClickInput()
        if self.input_OK == True:
            self.OnClickCalc()

    def on_options(self,event):
        suma = 0
        for i in range(0,len(self.BC)):
            suma = suma + sum(self.BC[i][1:4])
        
        self.n_modes = len(self.NOD)*2 - suma            
        dlg = Analysis_Options(self.PAR,self.n_modes)
        dlg.CenterOnScreen()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.PAR = dlg.dat_out
            self.accuracy = int(self.PAR[11]) + 1
        else:
            pass
        dlg.Destroy()      
          
    def create_main_panel(self):
        """ Creates the main panel with all the controls on it:
             * mpl canvas 
             * mpl navigation toolbar
             * Control panel for interaction
        """

        self.Bind(wx.EVT_CHAR_HOOK, self.OnKeyUP)

        # Create the mpl Figure and FigCanvas objects. 
        # 5x4 inches, 100 dots-per-inch
        #
        self.dpi = 100
        self.fig = Figure(dpi=self.dpi, tight_layout = True, facecolor = 'white')
        self.fig.set_size_inches(6, 6, forward=True)
        self.canvas = FigCanvas(self, -1, self.fig)
        
        # Since we have only one plot, we can use add_axes 
        # instead of add_subplot, but then the subplot
        # configuration tool in the navigation toolbar wouldn't
        # work.
        #
        
        #self.FirstFrame = FirstFrame()

        self.axes = self.fig.add_subplot(111, aspect='equal', frame_on = False)
        self.axes.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axes.set_ylim([self.Lim_Y1, self.Lim_Y2])
        self.axes.set_xticklabels([])
        self.axes.set_yticklabels([])
        self.axes.set_xticks(self.x_tick)
        self.axes.set_yticks(self.y_tick) 

        # Bind the 'pick' event for clicking on one of the bars
        #
        self.canvas.mpl_connect('pick_event', self.on_pick)
        self.canvas.mpl_connect('motion_notify_event', self.on_motion)
        self.canvas.mpl_connect('button_release_event', self.on_release)
        self.canvas.mpl_connect('button_press_event', self.on_canvas_click)

        # Create the navigation toolbar, tied to the canvas
        self.toolbar = NavigationToolbar(self.canvas)
        self.toolbar.Hide()
#    ==========  TOOLBAR   ==============================================
        self.el_draw = False
        self.sup_draw = False
        
        self.vzoom = False
        self.vpan = False

        self.Initialization()

        self.col1 = 'black'
        
        TBFLAGS = ( wx.TB_VERTICAL
#             | wx.NO_BORDER
            | wx.TB_FLAT
#             | wx.TB_TEXT
#             |wx.TB_NODIVIDER
#             | wx.TB_HORZ_LAYOUT
            )
        
        self.tb = self.CreateToolBar( TBFLAGS )
        tsize = (32,32)
        
        
        self.IDs = [10,20,30,40,50,60,70,80,90,100]
        
#         arrow_bmp = wx.Bitmap(self.PATH + '\icons\\Cursor_B.png')
        arrow_bmp = wx.Bitmap(self.PATH + '/icons/Cursor_B.png')
#        print('WWWWWWWWMLKKKK')
#        print(self.PATH)
#        print('WWWWWWWWMLKKKK')
        arrow_bmp = self.scale_bitmap(arrow_bmp, tsize)
        
        self.tb.AddCheckTool(self.IDs[0], "Select", arrow_bmp, shortHelp="Select")
        self.Bind(wx.EVT_TOOL, self.on_tool_1, id=self.IDs[0])
        
        self.tb.AddSeparator()

#         mat_bmp = wx.Bitmap(self.PATH + '\icons\\mat.png')
        mat_bmp = wx.Bitmap(self.PATH + '/icons/mat.png')
        self.tb.AddLabelTool(self.IDs[3], "Materials", mat_bmp, shortHelp="Materials")
        self.Bind(wx.EVT_TOOL, self.on_tool_4, id=self.IDs[3])

#         sec_bmp = wx.Bitmap(self.PATH + '\icons\\sec.png')
        sec_bmp = wx.Bitmap(self.PATH + '/icons/sec.png')
        self.tb.AddLabelTool(self.IDs[4], "Sections", sec_bmp, shortHelp="Sections")
        self.Bind(wx.EVT_TOOL, self.on_tool_5, id=self.IDs[4])

        self.tb.AddSeparator()  

#         element_bmp = wx.Bitmap(self.PATH + '\icons\\element.png')
        element_bmp = wx.Bitmap(self.PATH + '/icons/element.png')
        self.tb.AddCheckTool(self.IDs[1], "Draw Element", element_bmp, shortHelp="Draw Element")
        self.Bind(wx.EVT_TOOL, self.on_tool_2, id=self.IDs[1])

        self.tb.AddSeparator()

#         sup_bmp = wx.Bitmap(self.PATH + '\icons\\hinge.png')
        sup_bmp = wx.Bitmap(self.PATH + '/icons/hinge.png')
        self.tb.AddCheckTool(self.IDs[2], "Support", sup_bmp, shortHelp="Support")
        self.Bind(wx.EVT_TOOL, self.on_tool_3, id=self.IDs[2])    

        self.tb.AddSeparator()

#         f1_bmp = wx.Bitmap(self.PATH + '\icons\\nf.png')
        f1_bmp = wx.Bitmap(self.PATH + '/icons/nf.png')
        self.tb.AddCheckTool(self.IDs[5], "Loads on Node", f1_bmp, shortHelp="Loads on Node")
        self.Bind(wx.EVT_TOOL, self.on_tool_6, id=self.IDs[5])  
        
        self.tb.AddSeparator()
        
        calc_bmp = wx.Bitmap(self.PATH + '/icons/calc.png')
        self.tb.AddCheckTool(self.IDs[6], "Calculate", calc_bmp, shortHelp="Calculate")
        self.Bind(wx.EVT_TOOL, self.on_tool_7, id=self.IDs[6])  
        
        opt_bmp = wx.Bitmap(self.PATH + '/icons/opt.png')
        self.tb.AddCheckTool(self.IDs[7], "Optimize", opt_bmp, shortHelp="Optimize")
        self.Bind(wx.EVT_TOOL, self.on_tool_8, id=self.IDs[7])
# #         f2_bmp = wx.Bitmap(self.PATH + '\icons\\ef.png')
#         f2_bmp = wx.Bitmap(self.PATH + '/icons/ef.png')
#         self.tb.AddCheckTool(self.IDs[6], "Loads on Element", f2_bmp, shortHelp="Loads on Element")
#         self.Bind(wx.EVT_TOOL, self.on_tool_7, id=self.IDs[6])
#         
# #         f3_bmp = wx.Bitmap(self.PATH + '\icons\\dl.png')
#         f3_bmp = wx.Bitmap(self.PATH + '/icons/dl.png')
#         self.tb.AddCheckTool(self.IDs[7], "Distributed Load", f3_bmp, shortHelp="Distributed Load")
#         self.Bind(wx.EVT_TOOL, self.on_tool_8, id=self.IDs[7])  
#         
# #         f4_bmp = wx.Bitmap(self.PATH + '\icons\\tl.png')
#         f4_bmp = wx.Bitmap(self.PATH + '/icons/tl.png')
#         self.tb.AddCheckTool(self.IDs[8], "Temperature Load", f4_bmp, shortHelp="Temperature Load")
#         self.Bind(wx.EVT_TOOL, self.on_tool_9, id=self.IDs[8])  
#         
# #         f5_bmp = wx.Bitmap(self.PATH + '\icons\\sd.png')
#         f5_bmp = wx.Bitmap(self.PATH + '/icons/sd.png')
#         self.tb.AddCheckTool(self.IDs[9], "Support Displacement", f5_bmp, shortHelp="Support Displacement")
#         self.Bind(wx.EVT_TOOL, self.on_tool_10, id=self.IDs[9]) 
         
        self.tb.SetToolBitmapSize(tsize)
        self.tb.Realize()
#    ==========================================================================        
        
        #### NODES


        self.hbox = wx.BoxSizer(wx.HORIZONTAL)

        self.cp = cp = wx.CollapsiblePane(self, label="Data",style=wx.CP_DEFAULT_STYLE|wx.CP_NO_TLW_RESIZE)
        self.Bind(wx.EVT_COLLAPSIBLEPANE_CHANGED, self.OnPaneChanged, cp)
        self.MakePaneContent(cp.GetPane())
        self.cp.SetBackgroundColour("white")

        self.hbox.Add(self.canvas, 0, flag = wx.EXPAND | wx.ALL)
        self.hbox.Add(cp, 0, wx.ALIGN_LEFT | wx.ALL, 15)
#         self.hbox.Add(self.vbox2, 0, flag = wx.ALIGN_LEFT | wx.TOP)
        self.hbox.AddSpacer(10)

        self.SetSizer(self.hbox)
        self.hbox.Fit(self)

        self.Bind(wx.EVT_SIZE, self.OnSize)

    def on_tool_7(self,event):
        self.Create_Input_File('test_input.txt')
        os.chdir(self.PATH) #Set CWD for the fortran
        # Windows iOS
        Path = self.PATH + r'/bin/OpTrusDE'
#        Path = self.PATH + r'/OPT_Truss_DE.exe'
   
        subprocess.call(Path, shell=False)
         
        app.frame_out = OutputFrame(self.Lim_X1, self.Lim_Y1, self.Lim_X2, self.Lim_Y2, 
                                    self.NOD, self.ELE, self.BC, self.SEC, self.FOR1, self.PATH)
        app.frame_out.Show()
        
        val = app.frame_out.ShowModal()
        if val == wx.ID_OK:
            ELO = app.frame_out.ELE_opt
            SCM = np.array(self.SEC)
            ELE = np.array(self.ELE)
            for i in range(0,len(ELO[:,0])):
                Arele = ELO[i,1]
                if Arele not in SCM[:,1]:
                    idmat = SCM[ELE[i,3]-1,2]
                    idsec = SCM[-1,0]
                    SCM = np.append(SCM,[[idsec+1,Arele,idmat]],axis=0)
                    ELE[i,3] = idsec+1

            self.NOD = app.frame_out.NOD_opt.tolist()
            self.ELE = ELE.tolist()
            self.SEC = SCM.tolist()
            self.draw_figure()
        
        
                
#        print('gsggdgdgdggd')
#        print(app.frame_out.ACCEPT)
        
#        print(app.frame_out.EVT_CLOSE)
        

#         self.Create_Input_File('fgd.txt')
#         dlg = Materials(self.MAT,self.PATH)
#         dlg.CenterOnScreen()
#         val = dlg.ShowModal()
#         if val == wx.ID_OK:
#             R = self.gridMaterials.GetNumberRows()
#             if R > 0:
#                 self.gridMaterials.DeleteRows(pos=0, numRows=R, updateLabels=True)
#             self.MAT = dlg.MAT
#             self.gridMaterials.AppendRows(len(self.MAT), updateLabels = True)
#             for i in range(0,len(self.MAT)):
#                 self.gridMaterials.SetCellValue(i,0,str(self.MAT[i][1]))
#                 self.gridMaterials.SetCellValue(i,1,str(self.MAT[i][2]))
# #                 self.gridMaterials.SetCellValue(i,2,str(self.MAT[i][3]))
#         else:
#             pass
#         dlg.Destroy()
            
    def on_tool_8(self,event):
        suma = 0
        for i in range(0,len(self.BC)):
            suma = suma + sum(self.BC[i][1:4])
        
        self.n_modes = len(self.NOD)*2 - suma 
        dlg = Optimization_Options_DE(self.VARS,self.OBJ,self.CONSTR,self.OPT_PAR)
        dlg.CenterOnScreen()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.VARS    = dlg.vars_out
            self.OBJ     = dlg.obj_out
            self.CONSTR  = dlg.constr_out
            self.OPT_PAR = dlg.par_out
            
#             print self.VARS
#             print self.OBJ
#             print self.CONSTR
#             print self.OPT_PAR
        else:
            pass
        dlg.Destroy()        

    def on_tool_1(self,event):
        tb = event.GetEventObject()
        if tb.GetToolState(self.IDs[0]) == True:
            SRMenu = wx.Menu()
            
            item1 = wx.MenuItem(SRMenu, 1, "Element")
            item2 = wx.MenuItem(SRMenu, 2, "Support")
            item3 = wx.MenuItem(SRMenu, 3, "Force")
            
            SRMenu.AppendItem(item1)
            SRMenu.AppendItem(item2)
            SRMenu.AppendItem(item3)
            
            self.Bind(wx.EVT_MENU, self.OnSelectMenu, item1)
            self.Bind(wx.EVT_MENU, self.OnSelectMenu, item2)
            self.Bind(wx.EVT_MENU, self.OnSelectMenu, item3)
            
            self.PopupMenu(SRMenu)
            
            self.uncheck_tools(tb,self.IDs[0])
        else:
            self.Set_State(0)
            for i in range(0,self.n_elements):
                self.selected_el[i] = 0
            self.draw_figure()

    def OnSelectMenu(self,event):
        idx = event.GetId()
        if idx == 1:
            self.Set_State(31)
        elif idx == 2:
            self.Set_State(32)
        elif idx == 3:
            self.Set_State(33)
            
    def on_tool_2(self,event):
        tb = event.GetEventObject()            
        if tb.GetToolState(self.IDs[1]) == True:
            self.Set_State(1)
            self.uncheck_tools(tb,self.IDs[1])
        else:
            self.Set_State(0)
            self.sup_draw = False
            
    def on_tool_3(self,event):
        tb = event.GetEventObject()          
        if tb.GetToolState(self.IDs[2]) == True:
            dlg = Support([0, 1, 1, 0])
            dlg.CenterOnScreen()
            val = dlg.ShowModal()
            if val == wx.ID_OK:
                self.Set_State(4)
                self.triple = dlg.triple
                self.sup_angle = dlg.sup_angle
            else:
                pass
            dlg.Destroy()
            
            self.uncheck_tools(tb,self.IDs[2])
        else:
            if hasattr(self, 'temp_patch'):
                self.temp_patch.remove()
                del self.temp_patch
                self.canvas.draw()
                         
            self.Set_State(0)
            

    def on_tool_4(self,event):
        dlg = Materials(self.MAT,self.PATH)
        dlg.CenterOnScreen()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.MAT = dlg.MAT
            self.UpdateGridMat()
        else:
            pass
        dlg.Destroy()

    def on_tool_5(self,event):
        dlg = Sections(self.SEC,self.PATH)
        dlg.CenterOnScreen()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.SEC = dlg.SEC
            self.UpdateGridSec()
                
        else:
            pass
        dlg.Destroy()
 
    def UpdateGridMat(self): 
        R = self.gridMaterials.GetNumberRows()
        if R > 0:
            self.gridMaterials.DeleteRows(pos=0, numRows=R, updateLabels=True)
        self.gridMaterials.AppendRows(len(self.MAT), updateLabels = True)
        for i in range(0,len(self.MAT)):
            self.gridMaterials.SetCellValue(i,0,str(self.MAT[i][1]))
            self.gridMaterials.SetCellValue(i,1,str(self.MAT[i][2]))  
       
    def UpdateGridSec(self):
        R = self.gridSections.GetNumberRows()
        if R > 0:
            self.gridSections.DeleteRows(pos=0, numRows=R, updateLabels=True)
        self.gridSections.AppendRows(len(self.SEC), updateLabels = True)
        for i in range(0,len(self.SEC)):
            self.gridSections.SetCellValue(i,0,str(self.SEC[i][1]))
            self.gridSections.SetCellValue(i,1,str(self.SEC[i][2]))

    def UpdateGridNod(self): 
        R = self.gridNodes.GetNumberRows()
        if R > 0:
            self.gridNodes.DeleteRows(pos=0, numRows=R, updateLabels=True)
        self.gridNodes.AppendRows(len(self.NOD), updateLabels = True)
        for i in range(0,len(self.NOD)):
            self.gridNodes.SetCellValue(i,0,str(self.NOD[i][1]))
            self.gridNodes.SetCellValue(i,1,str(self.NOD[i][2]))  
            
    def UpdateGridEle(self): 
        R = self.gridElements.GetNumberRows()
        if R > 0:
            self.gridElements.DeleteRows(pos=0, numRows=R, updateLabels=True)
        self.gridElements.AppendRows(len(self.ELE), updateLabels = True)
        for i in range(0,len(self.ELE)):
            self.gridElements.SetCellValue(i,0,str(self.ELE[i][1]))
            self.gridElements.SetCellValue(i,1,str(self.ELE[i][2])) 
            self.gridElements.SetCellValue(i,2,str(self.ELE[i][3])) 
    

    def on_tool_6(self,event):
        tb = event.GetEventObject()            
        if tb.GetToolState(self.IDs[5]) == True:
            dlg = F1([0, 0, 0, 0],self.PATH)
            dlg.CenterOnScreen()
            val = dlg.ShowModal()
            if val == wx.ID_OK:
                self.Set_State(5)
                self.F1 = dlg.dat_out
            else:
                pass
            dlg.Destroy()
            self.uncheck_tools(tb,self.IDs[5])
        else:
            if hasattr(self, 'temp_F1'):
                self.temp_F1.remove()
                del self.temp_F1
            if hasattr(self, 'temp_F1M'):
                self.temp_F1M.remove()
                del self.temp_F1M
                self.canvas.draw()
            self.Set_State(0)
            
    def OnPaneChanged(self, evt=None):
        # redo the layout
        
        self.UpdateGridSec()
        self.UpdateGridNod()
        self.UpdateGridEle()
        
        s = self.Glob_size
        if self.cp.IsExpanded():
            self.SetSize((s[0]+420, s[1]))
        else:
            self.SetSize((s[0]-420, s[1]))
            
        self.Layout()
        

    def OnSize(self, event):
        self.Glob_size = event.GetSize()

    def MakePaneContent(self, pane):
        pane.SetBackgroundColour("white")
        self.nb = wx.Notebook(pane)
        
        self.PropertieTab = wx.Window(self.nb)
        self.GeometryTab = wx.Window(self.nb)
        self.BCTab = wx.Window(self.nb)
#         self.ForceTab = wx.Window(self.nb)
        
        self.nb.AddPage(self.PropertieTab, "Properties")
        self.nb.AddPage(self.GeometryTab, "Geometry")
        self.nb.AddPage(self.BCTab, "Fixities and Loads")
#         self.nb.AddPage(self.ForceTab, "Loads")
        
        self.Properties(self.nb) 
        self.Geometry(self.nb)
        self.BCs_Loads(self.nb)        
#         self.Forces(self.nb)
        
        self.buttonDraw = wx.Button(pane, label="Draw")
        self.Bind(wx.EVT_BUTTON, self.OnClickDraw, self.buttonDraw)
        
        self.vbox2 = wx.BoxSizer(wx.VERTICAL)
        self.vbox2.Add(self.nb, 5, flag =  wx.ALL  | wx.ALIGN_CENTER_VERTICAL)
        self.vbox2.AddSpacer(15)
        self.vbox2.Add(self.buttonDraw, 5, wx.ALL | wx.GROW | wx.ALIGN_CENTER_VERTICAL)

        self.PropertieTab.SetSizer(self.vbox2S)
        self.GeometryTab.SetSizer(self.vbox2G)
        self.BCTab.SetSizer(self.vbox2B)
#         self.ForceTab.SetSizer(self.vbox2F)
        
        pane.SetSizer(self.vbox2)

    def scale_bitmap(self, bitmap, size):
        width = size[0]
        height = size[1]
#        image = wx.ImageFromBitmap(bitmap)
        image = wx.Bitmap.ConvertToImage(bitmap)
        image = image.Scale(width, height, wx.IMAGE_QUALITY_HIGH)
        result = wx.BitmapFromImage(image)
        return result

    def OnClickDraw(self, event):
        self.draw_figure()

    def on_pick(self, event):
        obj = event.artist
        if self.case == 31:
            if isinstance(obj, matplotlib.lines.Line2D):
                x, y = obj.get_data()
                idx = self.get_element_ID(self.get_node_ID(x[0],y[0]),self.get_node_ID(x[1],y[1]))
                if self.selected_el[idx-1] == 0:
                    self.selected_el[idx-1] = 1
                elif self.selected_el[idx-1] == 1:
                    self.selected_el[idx-1] = 0
                self.draw_figure()
                self.Set_State(31)
        elif self.case == 32:
            idx = 0
            if isinstance(obj, matplotlib.patches.PathPatch):
                path = obj.get_path()
                for i in range(0,self.n_nodes):
                    x0 = self.patch_box[0]
                    y0 = self.patch_box[1]
                    x1 = self.patch_box[2]
                    y1 = self.patch_box[3]
                    px = self.NOD[i][1]
                    py = self.NOD[i][2]
                    if x0 < px and px < x1 and y0 < py and py < y1:
                        idx = self.NOD[i][0]
                
                if not self.sup_selected[0]:
                    self.sup_selected = [True,idx]
                    self.Set_State(32)
                else:
                    self.sup_selected = [False,idx]
                    self.Set_State(32)
                self.draw_figure()
                
        elif self.case == 33:
            idx = 0
            if isinstance(obj, matplotlib.patches.PathPatch):
                cond_f1 = False
                cond_f2 = False
                cond_f3 = False
                path = obj.get_path()
                verts = path.vertices
                for i in range(0,len(self.FOR1)):
                    px = self.NOD[int(self.FOR1[i][0])-1][1]
                    py = self.NOD[int(self.FOR1[i][0])-1][2]
                    if [px,py] in verts:
                        cond_f1 = True
                        idx = i                
                if cond_f1:
                    if not self.f1_selected[0]:
                        self.f1_selected = [True,idx]
                        self.Set_State(33)
                    else:
                        self.f1_selected = [False,idx]
                        self.Set_State(33)
                        
                for i in range(0,len(self.FOR2)):
                    [x1_el,y1_el,x2_el,y2_el] = self.get_element_properties(self.FOR2[i][0]-1,['x1','y1','x2','y2'])
                    px = x1_el + (x2_el-x1_el)*self.FOR2[i][4]
                    py = y1_el + (y2_el-y1_el)*self.FOR2[i][4]
                    if [px,py] in verts:
                        cond_f2 = True
                        idx = i
                if cond_f2:
                    if not self.f2_selected[0]:
                        self.f2_selected = [True,idx]
                        self.Set_State(33)
                    else:
                        self.f2_selected = [False,idx]
                        self.Set_State(33)
                        
                for i in range(0,len(self.FOR3)):
#                     [x1_el,y1_el,x2_el,y2_el] = self.get_element_properties(self.FOR3[i][0]-1,['x1','y1','x2','y2'])
#                     px1 = x1_el
#                     py1 = y1_el
#                     px2 = x2_el
#                     py2 = y2_el
                    if path.contains_point([self.x_click,self.y_click]):
                        print('yes')
                        cond_f3 = True
                        idx = i
                if cond_f3:
                    if not self.f3_selected[0]:
                        self.f3_selected = [True,idx]
                        self.Set_State(33)
                    else:
                        self.f3_selected = [False,idx]
                        self.Set_State(33)                      

                self.draw_figure()
                
        elif self.case == 6 or self.case == 7 or self.case == 8:
            if isinstance(obj, matplotlib.lines.Line2D):
                x, y = obj.get_data()
                self.cur_ele_x = x
                self.cur_ele_y = y


    def get_node_ID(self,x,y):
        for i in range(0,self.n_nodes):
            if self.NOD[i][1] == x and self.NOD[i][2] == y:
                idx = self.NOD[i][0]
        
        return idx
    
    def get_element_ID(self,n1,n2):
        for i in range(0,self.n_elements):
            if self.ELE[i][1] == n1 and self.ELE[i][2] == n2:
                idx = self.ELE[i][0]
        
        return idx

    def get_support_ID(self,node_ID):
        for i in range(0,self.n_supports):
            if self.BC[i][0] == node_ID:
                idx = i
        
        return idx

    def get_f1_ID(self,node_ID):
        for i in range(0,len(self.FOR1)):
            if self.FOR1[i][0] == node_ID:
                idx = i
        
        return idx
    
#     def get_f2_ID(self,node_ID):
#         for i in range(0,len(self.FOR2)):
#             if self.FOR2[i][0] == node_ID:
#                 idx = i
#         
#         return idx
#     
#     def get_f3_ID(self,node_ID):
#         for i in range(0,len(self.FOR3)):
#             if self.FOR3[i][0] == node_ID:
#                 idx = i
#         
#         return idx
#     
#     def get_f4_ID(self,node_ID):
#         for i in range(0,len(self.FOR4)):
#             if self.FOR4[i][0] == node_ID:
#                 idx = i
#         
#         return idx
# 
#     def get_f5_ID(self,node_ID):
#         for i in range(0,len(self.FOR5)):
#             if self.FOR5[i][0] == node_ID:
#                 idx = i
#         
#         return idx
#     
#     def get_f6_ID(self,node_ID):
#         for i in range(0,len(self.FOR6)):
#             if self.FOR6[i][0] == node_ID:
#                 idx = i
#         
#         return idx

    def get_element_properties(self, ind, props):
        idx = int(ind)
        x1, y1 = self.NOD[self.ELE[idx][1]-1][1], self.NOD[self.ELE[idx][1]-1][2]
        x2, y2 = self.NOD[self.ELE[idx][2]-1][1], self.NOD[self.ELE[idx][2]-1][2]
         
        dx = x2 - x1
        dy = y2 - y1
        
        if 'L' in props:
            L = (dx**2 + dy**2)**0.5
        
        if 'phi' in props:
            if dx != 0:
                if (dy >= 0) and (dx > 0):
                    phi = np.arctan(dy/dx)
                elif (dy >= 0) and (dx < 0):
                    phi = np.pi - np.arctan(abs(dy/dx))
                elif (dy <= 0) and (dx > 0):
                    phi = 2*np.pi - np.arctan(abs(dy/dx))
                elif (dy <= 0) and (dx < 0):
                    phi = np.pi + np.arctan(abs(dy/dx))       
            else:
                if dy <= 0:
                    phi = 3*np.pi/2
                else:
                    phi = np.pi/2
        
        properties = []
        for i in range(0,len(props)):
            properties.append(eval(props[i]))
            
        return properties

    def on_release(self, event):
        if self.case == 31:
            self.Set_State(31)
        elif self.case == 31:
            self.Set_State(32)
        elif self.case == 33:
            self.Set_State(33)
        
        if self.vzoom == True:
            self.toolbar.zoom()
            self.vzoom = False

    def on_motion(self, event):
        cursor = wx.StockCursor(wx.CURSOR_ARROW)
        GS = self.Grid_Spacing
        x_init = event.xdata
        y_init = event.ydata
        
        if isinstance(x_init, float) == True and isinstance(y_init, float) == True:
            x = int(GS*100000*round(float(x_init*100000)/GS/100000))/100000.0
            y = int(GS*100000*round(float(y_init*100000)/GS/100000))/100000.0
            text = 'X = ' + "%.2f"%x_init + ', Y = ' + "%.2f"%y_init
            self.SetStatusText(text,0)

            if self.case == 1 or self.case == 2:
                cursor = wx.StockCursor(wx.CURSOR_CROSS)
                if self.case == 2:
                    if hasattr(self, 'temp'):
                        if len(self.temp) != 0:
                            self.temp.pop(0).remove()
                    self.temp = self.axes.plot([self.n1[0],x],[self.n1[1],y], color = 'black',linewidth=1.5)
                    self.canvas.draw()
            elif self.case == 4:
                cursor = wx.StockCursor(wx.CURSOR_CROSS)
                if hasattr(self, 'temp_patch'):
                    self.temp_patch.remove()
                    del self.temp_patch
                    
#                 if self.triple[0] == 1 and self.triple[1] == 1 and self.triple[2] == 1:
#                     patch = self.get_patch_fixed([x,y],0.03*self.Dim_X,0,'grey')
#                 elif self.triple[0] == 1 and self.triple[1] == 1 and self.triple[2] == 0:
#                     patch = self.get_patch_hinge([x,y],0.03*self.Dim_X,0,'grey')
#                 else:
#                     patch = self.get_patch_roller([x,y],0.03*self.Dim_X,0,'grey')

                if self.triple[0] == 1 and self.triple[1] == 1:
                    patch = self.get_patch_hinge([x,y],0.03*self.Dim_X,0,'grey')
                else:
                    patch = self.get_patch_roller([x,y],0.03*self.Dim_X,0,'grey')
                    
                self.temp_patch = self.axes.add_patch(patch)
                self.canvas.draw()
            elif self.case == 5:
                cursor = wx.StockCursor(wx.CURSOR_CROSS)
                if hasattr(self, 'temp_F1'):
                    self.temp_F1.remove()
                    del self.temp_F1
                if hasattr(self, 'temp_F1M'):
                    self.temp_F1M.remove()
                    del self.temp_F1M
                
                if self.F1[0] != 0 or self.F1[1] != 0:
                    scl = 0.005*self.Dim_X
                    f1 = scl*self.F1[0]
                    f2 = scl*self.F1[1]
                    if f1 > 0 and f2 >= 0:
                        ang = np.arctan(f2/f1) - np.pi/2
                    elif f1 < 0 and f2 >= 0:
                        ang = np.arctan(f2/f1) + np.pi/2
                    elif f1 < 0 and f2 <= 0:
                        ang = np.arctan(f2/f1) + np.pi/2
                    elif f1 > 0 and f2 <= 0:
                        ang = np.arctan(f2/f1) - np.pi/2
                    elif f1 == 0 and f2 >= 0: 
                        ang = 0
                    elif f1 == 0 and f2 <= 0: 
                        ang = np.pi
                        
                    self.patchF1 = self.get_patch_arrow((x,y), 0.03*self.Dim_X, ang, (x-f1,y-f2), 'blue')
                    self.temp_F1 = self.axes.add_patch(self.patchF1)
                else:
                    ang = 0
                
                if self.F1[2] != 0:
                    self.patchM1 = self.get_patch_moment((x,y), 0.02*self.Dim_X, ang+np.pi, (0,0), 'blue',False)
                    self.temp_F1M = self.axes.add_patch(self.patchM1)
                
                self.canvas.draw()

            elif self.case == 6:
                cursor = wx.StockCursor(wx.CURSOR_CROSS)
                if hasattr(self, 'temp_F2'):
                    self.temp_F2.remove()
                    del self.temp_F2
                if hasattr(self, 'temp_F2M'):
                    self.temp_F2M.remove()
                    del self.temp_F2M
                
                if self.F2[0] != 0 or self.F2[1] != 0:
                    scl = 0.005*self.Dim_X
                    f1 = scl*self.F2[0]
                    f2 = scl*self.F2[1]
                    if f1 > 0 and f2 >= 0:
                        ang = np.arctan(f2/f1) - np.pi/2
                    elif f1 < 0 and f2 >= 0:
                        ang = np.arctan(f2/f1) + np.pi/2
                    elif f1 < 0 and f2 <= 0:
                        ang = np.arctan(f2/f1) + np.pi/2
                    elif f1 > 0 and f2 <= 0:
                        ang = np.arctan(f2/f1) - np.pi/2
                    elif f1 == 0 and f2 >= 0: 
                        ang = 0
                    elif f1 == 0 and f2 <= 0: 
                        ang = np.pi
                        
                    self.patchF2 = self.get_patch_arrow((x_init,y_init), 0.03*self.Dim_X, ang, (x_init-f1,y_init-f2), 'blue')
                    self.temp_F2 = self.axes.add_patch(self.patchF2)
                else:
                    ang = 0
                 
                if self.F2[2] != 0:
                    self.patchM2 = self.get_patch_moment((x_init,y_init), 0.02*self.Dim_X, ang+np.pi, (0,0), 'blue',False)
                    self.temp_F2M = self.axes.add_patch(self.patchM2)
                 
                self.canvas.draw()
            
            elif self.case == 7:
                cursor = wx.StockCursor(wx.CURSOR_CROSS)
                if hasattr(self, 'temp_F3'):
                    self.temp_F3.remove()
                    del self.temp_F3
                
                if self.F3[0] != 0 or self.F3[1] != 0:
                    scl = 0.005*self.Dim_X
                    f1 = scl*self.F3[0]
                    f2 = scl*self.F3[1]
                    if f1 > 0 and f2 >= 0:
                        ang = np.arctan(f2/f1) - np.pi/2
                    elif f1 < 0 and f2 >= 0:
                        ang = np.arctan(f2/f1) + np.pi/2
                    elif f1 < 0 and f2 <= 0:
                        ang = np.arctan(f2/f1) + np.pi/2
                    elif f1 > 0 and f2 <= 0:
                        ang = np.arctan(f2/f1) - np.pi/2
                    elif f1 == 0 and f2 >= 0: 
                        ang = 0
                    elif f1 == 0 and f2 <= 0: 
                        ang = np.pi
                    
#                     [x1_el,y1_el,x2_el,y2_el,phi] = self.get_element_properties(self.FOR2[i][0]-1,['x1','y1','x2','y2','phi'])
                    
                    self.patchF3 = self.get_patch_distributed(x_init,y_init,x_init+self.Dim_X/5,y_init,np.pi,10,0.03*self.Dim_X,'blue')
                    self.temp_F3 = self.axes.add_patch(self.patchF3)

                self.canvas.draw()

            elif self.case == 8:
                cursor = wx.StockCursor(wx.CURSOR_CROSS)
                if hasattr(self, 'temp_F41'):
                    self.temp_F41.remove()
                    del self.temp_F41
                if hasattr(self, 'temp_F42'):
                    self.temp_F42.remove()
                    del self.temp_F42
                
                if self.F4[0] != 0 or self.F4[1] != 0 or self.F4[2] != 0:
                    self.patchF41, self.patchF42 = self.get_patch_temperature(x_init,y_init,x_init+self.Dim_X/12,y_init,0)
                    self.temp_F41 = self.axes.add_patch(self.patchF41)
                    self.temp_F42 = self.axes.add_patch(self.patchF42)
    
                    self.canvas.draw()

            elif self.case == 9:
                cursor = wx.StockCursor(wx.CURSOR_CROSS)
                if hasattr(self, 'temp_F51'):
                    self.temp_F51.pop(0).remove()
                    del self.temp_F51
                if hasattr(self, 'temp_F52'):
                    self.temp_F52.pop(0).remove()
                    del self.temp_F52
                    
                x_2, y_2 = x_init, y_init - self.Dim_X/20
                x_3, y_3 = x_2 + self.Dim_X/30, y_2
                x_4, y_4 = x_2 - self.Dim_X/30, y_2
                self.temp_F51 = self.axes.plot([x_init,x_2],[y_init,y_2],'grey',linestyle='--',linewidth=2)
                self.temp_F52 = self.axes.plot([x_3,x_4],[y_3,y_4],'grey',linewidth=2)
                
                self.canvas.draw()

            elif self.vpan == True:
                cursor = wx.StockCursor(wx.CURSOR_HAND)
            elif self.vzoom == True:
                cursor = wx.StockCursor(wx.CURSOR_BULLSEYE)
            else:
                cursor = wx.StockCursor(wx.CURSOR_ARROW)
        else:
            self.SetStatusText('',0)
        self.canvas.SetCursor(cursor)  

    def get_patch_fixed(self, position, scale, angle, col):
        verts = [(-1.25,  0.0), (-1.0,  0.0), (-0.5, -0.3), (-1.0,  0.0), (-0.5,  0.0), 
                 (0.0  , -0.3), (-0.5,  0.0), (0.0 ,  0.0), (0.5 , -0.3), (0.0 ,  0.0), 
                 (0.5  ,  0.0), (1.0 , -0.3), (0.5 ,  0.0), (1.0 ,  0.0), (1.5 , -0.3), 
                 (1.0  ,  0.0), (1.5 ,  0.0), (0.0 ,  0.0)]
         
        codes = [Path.MOVETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO]
        
        angle = np.pi*angle/180
        vertsT = verts[:]
        for i in range(0,len(verts)):
            vertsT[i] = (verts[i][0]*np.cos(angle)-verts[i][1]*np.sin(angle), verts[i][0]*np.sin(angle)+verts[i][1]*np.cos(angle))
        
        for i in range(0,len(verts)):
            vertsT[i] = (scale*vertsT[i][0],scale*vertsT[i][1])
    
        for i in range(0,len(verts)):
            vertsT[i] = (vertsT[i][0] + position[0],vertsT[i][1] + position[1])
     
        pathT = Path(vertsT, codes)
        hingeT = patches.PathPatch(pathT, facecolor='none', lw=1, edgecolor=col,picker = self.patch_picker)
        return hingeT

    def get_patch_hinge(self, position, scale, angle, col):
        verts = [(-1.25, -0.75), (-1.0, -0.75), (-0.5 , -1.05), (-1.0, -0.75), (-0.5, -0.75), 
                 (0.0  , -1.05), (-0.5, -0.75), (0.0  , -0.75), (0.5 , -1.05), (0.0, -0.75 ), 
                 (0.5  , -0.75), (1.0 , -1.05), (0.5  , -0.75), (1.0 , -0.75), (1.5, -1.05 ), 
                 (1.0  , -0.75), (1.5 , -0.75), (-0.75, -0.75), (0.0 , 0.0  ), (0.75, -0.75), (0.0 , 0.0  )]
        
        codes = [Path.MOVETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO]

        angle = np.pi*angle/180
        vertsT = verts[:]
        for i in range(0,len(verts)):
            vertsT[i] = (verts[i][0]*np.cos(angle)-verts[i][1]*np.sin(angle), verts[i][0]*np.sin(angle)+verts[i][1]*np.cos(angle))
        
        for i in range(0,len(verts)):
            vertsT[i] = (scale*vertsT[i][0],scale*vertsT[i][1])
    
        for i in range(0,len(verts)):
            vertsT[i] = (vertsT[i][0] + position[0],vertsT[i][1] + position[1])
     
        pathT = Path(vertsT, codes)
        hingeT = patches.PathPatch(pathT, facecolor='none', lw=1, edgecolor=col,picker = self.patch_picker)
        return hingeT

    def get_patch_roller(self, position, scale, angle, col):
        verts = [(-1.25, -1.  ), (-1.0 , -1.  ), (-0.5 , -1.30), (-1.0 , -1.  ), (-0.5, -1. ), 
                 (0.0  , -1.30), (-0.5 , -1.  ), (0.0  , -1.  ), (0.5  , -1.30), (0.0, -1.  ), 
                 (0.5  , -1.  ), (1.0  , -1.30), (0.5  , -1.  ), (1.0  , -1.  ), (1.5, -1.30), 
                 (1.0  , -1.  ), (1.5  , -1.  ), (-0.75, -0.75), (-0.75, -0.75), (0.0, 0.0  ), 
                 (0.75 , -0.75), (-0.75, -0.75), (0.0, 0.0  )]
    
        codes = [Path.MOVETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, 
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.MOVETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO,Path.LINETO]
        
        angle = np.pi*angle/180
        vertsT = verts[:]
        for i in range(0,len(verts)):
            vertsT[i] = (verts[i][0]*np.cos(angle)-verts[i][1]*np.sin(angle), verts[i][0]*np.sin(angle)+verts[i][1]*np.cos(angle))
        
        for i in range(0,len(verts)):
            vertsT[i] = (scale*vertsT[i][0],scale*vertsT[i][1])
    
        for i in range(0,len(verts)):
            vertsT[i] = (vertsT[i][0] + position[0],vertsT[i][1] + position[1])
     
        pathT = Path(vertsT, codes)
        hingeT = patches.PathPatch(pathT, facecolor='none', lw=1, edgecolor=col,picker = self.patch_picker)
        return hingeT

    def get_patch_arrow(self, position, scale, angle, tail, col):
        verts = [(-0.25, -0.5  ), (0.0 , 0.0  ), (0.25, -0.5  ), (0.0 , 0.0 )]
        codes = [Path.MOVETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO]
        vertsT = verts[:]
        for i in range(0,len(verts)):
            vertsT[i] = (verts[i][0]*np.cos(angle)-verts[i][1]*np.sin(angle), verts[i][0]*np.sin(angle)+verts[i][1]*np.cos(angle))

        for i in range(0,len(verts)):
            vertsT[i] = (scale*vertsT[i][0],scale*vertsT[i][1])
    
        for i in range(0,len(verts)):
            vertsT[i] = (vertsT[i][0] + position[0],vertsT[i][1] + position[1])

        vertsT.append(tail)
        pathT = Path(vertsT, codes)
        arrow = patches.PathPatch(pathT, facecolor='none', lw=1, edgecolor=col,alpha=0.8,picker = 5)
        return arrow

    def get_patch_moment(self, position, scale, angle, tail, col, pick):
        verts = [(1.5 , 1.5  ), (0.0, 3.0  ), (-1.5 , 1.5  ), (-1.3706, 1.9829), (-1.5 , 1.5  ), (-1.0171, 1.6294)]
        codes = [Path.MOVETO, Path.CURVE3, Path.CURVE3, Path.LINETO, Path.LINETO, Path.LINETO]
        vertsT = verts[:]
        for i in range(0,len(verts)):
            vertsT[i] = (verts[i][0]*np.cos(angle)-verts[i][1]*np.sin(angle), verts[i][0]*np.sin(angle)+verts[i][1]*np.cos(angle))

        for i in range(0,len(verts)):
            vertsT[i] = (scale*vertsT[i][0],scale*vertsT[i][1])
    
        for i in range(0,len(verts)):
            vertsT[i] = (vertsT[i][0] + position[0],vertsT[i][1] + position[1])

        pathT = Path(vertsT, codes)
        if pick:
            arrow = patches.PathPatch(pathT, facecolor='none', lw=1, edgecolor=col,alpha=0.8,picker = 5)
        else:
            arrow = patches.PathPatch(pathT, facecolor='none', lw=1, edgecolor=col,alpha=0.8)
            
        return arrow

    def get_patch_distributed(self,x1,y1,x2,y2,phi,val,scale,col):
        scl = 0.005*self.Dim_X
        arrow = [(-0.25, -0.5  ), (0.0 , 0.0  ), (0.25, -0.5  ), (0.0 , 0.0 )]
        arrowT = arrow[:]
        for i in range(0,len(arrow)):
            arrowT[i] = (arrow[i][0]*np.cos(phi)-arrow[i][1]*np.sin(phi), arrow[i][0]*np.sin(phi)+arrow[i][1]*np.cos(phi))

        for i in range(0,len(arrow)):
            arrowT[i] = (scale*arrowT[i][0],scale*arrowT[i][1])

        for i in range(0,len(arrow)):
            arrowT[i] = (arrowT[i][0] + x1,arrowT[i][1] + y1)

        arrowF = arrowT[:]
        xyT1 = (x1+val*scl*np.sin(phi), y1-val*scl*np.cos(phi))
        arrowF.append(xyT1)
        
        n_ar = int(((x2-x1)**2 + (y2-y1)**2)**0.5/self.Dim_X*10)
        step_x = (x2-x1)/n_ar
        step_y = (y2-y1)/n_ar
        for i in range(1,n_ar+1):
            x_ = x1+i*step_x
            y_ = y1+i*step_y
            xyT = (x_+val*scl*np.sin(phi), y_-val*scl*np.cos(phi))
            arrowF.append(xyT)
            arrowF.append((x_,y_))
            for j in range(0,len(arrowT)):
                arrowF.append((arrowT[j][0] + i*step_x,arrowT[j][1] + i*step_y))
            arrowF.append(xyT)
            
        codes = [Path.MOVETO]
        for i in range(0,len(arrowF)-1):
            codes.append(Path.LINETO)
        
        pathT = Path(arrowF, codes)    
        distr = patches.PathPatch(pathT, facecolor='none', lw=1, edgecolor=col,alpha=0.8,picker = 5)    
        
        return distr    

    def get_patch_temperature(self,x1,y1,x2,y2,phi):
        xyT1 = (x1-self.Dim_X/100*np.sin(phi), y1+self.Dim_X/100*np.cos(phi))
        xyT2 = (x2-self.Dim_X/100*np.sin(phi), y2+self.Dim_X/100*np.cos(phi))
        xyT3 = (x1+self.Dim_X/100*np.sin(phi), y1-self.Dim_X/100*np.cos(phi))
        xyT4 = (x2+self.Dim_X/100*np.sin(phi), y2-self.Dim_X/100*np.cos(phi))
         
        pathT = Path([(x1,y1),(x2,y2),xyT2,xyT1,(x1,y1)], 
                 [Path.MOVETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO])
        rect1 = patches.PathPatch(pathT, edgecolor = 'none', facecolor='blue', lw=1, alpha=0.3, picker = 5)

        pathT = Path([(x1,y1),(x2,y2),xyT4,xyT3,(x1,y1)], 
                 [Path.MOVETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO])
        rect2 = patches.PathPatch(pathT, edgecolor = 'none', facecolor='red', lw=1, alpha=0.3, picker = 5)

        return rect1, rect2

    def patch_picker(self, patch, mouseevent):
        px = mouseevent.xdata
        py = mouseevent.ydata
        path = patch.get_path()
        box = path.get_extents()
        d = self.Dim_X*0.02
        x0 = box.x0 - d
        y0 = box.y0 - d
        x1 = box.x1 + d
        y1 = box.y1 + d
        self.patch_box = [x0,y0,x1,y1]
        if x0 < px and px < x1 and y0 < py and py < y1:
            return True, dict()
        else:
            return False, dict()

    def patch_picker_load(self, patch, mouseevent):     
        px = mouseevent.xdata
        py = mouseevent.ydata
        path = patch.get_path()
        
        point = np.zeros((1,2))
        point[0][0] = px
        point[0][1] = py
        inside = path.contains_points(point)
        if inside == True:

            return True, dict()
        else:
            return False, dict()

    def OnKeyUP(self, event):
        keyCode = event.GetKeyCode()
        if keyCode == wx.WXK_ESCAPE:
            if self.case == 1:
                self.Set_State(0)
            elif self.case == 2:
                self.Set_State(1)
                if hasattr(self, 'temp'):
                    if len(self.temp) != 0:
                        self.temp.pop(0).remove()
                        self.canvas.draw()
                if hasattr(self, 'scat_n1'):
                    self.scat_n1.remove()
                    self.canvas.draw()
            elif self.case == 31:
                for i in range(0,self.n_elements):
                    self.selected_el[i] = 0
                self.draw_figure()
            elif self.case == 4:
                self.Set_State(0)
                if hasattr(self, 'temp_patch'):
                    self.temp_patch.remove()
                    del self.temp_patch
                    self.canvas.draw()
                    self.tb.ToggleTool(self.IDs[2],False)
            elif self.case == 5:
                self.Set_State(0)
                if hasattr(self, 'temp_F1'):
                    self.temp_F1.remove()
                    del self.temp_F1
                    self.canvas.draw()
                    self.tb.ToggleTool(self.IDs[5],False)
                if hasattr(self, 'temp_F1M'):
                    self.temp_F1M.remove()
                    del self.temp_F1M
                    self.canvas.draw()
                    self.tb.ToggleTool(self.IDs[5],False)
            elif self.case == 6:
                self.Set_State(0)
                if hasattr(self, 'temp_F2'):
                    self.temp_F2.remove()
                    del self.temp_F2
                    self.canvas.draw()
                    self.tb.ToggleTool(self.IDs[6],False)
                if hasattr(self, 'temp_F2M'):
                    self.temp_F2M.remove()
                    del self.temp_F2M
                    self.canvas.draw()
                    self.tb.ToggleTool(self.IDs[6],False)
            elif self.case == 7:
                self.Set_State(0)
                if hasattr(self, 'temp_F3'):
                    self.temp_F3.remove()
                    del self.temp_F3
                    self.canvas.draw()
                    self.tb.ToggleTool(self.IDs[7],False)
                    
        elif keyCode == wx.WXK_DELETE:
            if self.case == 31:
                Dead_el = []
                for i in range(0,self.n_elements):
                    if self.selected_el[i] == 1:
                        Dead_el.append(i)
                
                for i in range(0,len(Dead_el)):
                    self.Ele_to_del = Dead_el[i]
                    self.OnDelEle(0)
                    for j in range(i+1,len(Dead_el)):
                        Dead_el[j] = Dead_el[j] - 1
                self.draw_figure()
            elif self.case == 32:
                if self.sup_selected[0] == True:
                    self.sup_selected[0] = False
                    idx = self.get_support_ID(self.sup_selected[1])
                    self.BC_to_del = idx
                    self.OnDelBC(0)
                    self.draw_figure()
            elif self.case == 33:
                if self.f1_selected[0] == True:
                    self.f1_selected[0] = False
                    idx = self.f1_selected[1]
                    del self.FOR1[idx]
                    self.draw_figure()
    
                if self.f2_selected[0] == True:
                    self.f2_selected[0] = False
                    idx = self.f2_selected[1]
                    del self.FOR2[idx]
                    del self.f2_CS[idx]
                    self.draw_figure()
                    
                if self.f3_selected[0] == True:
                    self.f3_selected[0] = False
                    idx = self.f3_selected[1]
                    del self.FOR3[idx]
                    del self.f3_CS[idx]
                    self.draw_figure()
                 
        event.Skip()
        
    def on_canvas_click(self, event):
        self.x_click = event.xdata
        self.y_click = event.ydata
        GS = self.Grid_Spacing
        if self.case == 1:
            x = int(GS*100000*round(float(event.xdata*100000)/GS/100000))/100000.0
            y = int(GS*100000*round(float(event.ydata*100000)/GS/100000))/100000.0
            self.n1 = (x,y)
            self.n1_OK()
        elif self.case == 2:
            x = int(GS*100000*round(float(event.xdata*100000)/GS/100000))/100000.0
            y = int(GS*100000*round(float(event.ydata*100000)/GS/100000))/100000.0
            self.n2 = (x,y)
            self.n2_OK()
        elif self.case == 4:
            dist = self.Dim_X
            for i in range(0,self.n_nodes):
                dx = event.xdata - self.NOD[i][1]
                dy = event.ydata - self.NOD[i][2]
                t_dist = (dx**2 + dy**2)**0.5
                if t_dist < dist:
                    dist = t_dist
                    idx = i
                    
            if dist < 0.05*self.Dim_X:
                xp = self.NOD[idx][1]
                yp = self.NOD[idx][2]
                node_ID = self.get_node_ID(xp,yp)
                self.append_support(node_ID,self.triple,self.sup_angle)
                if self.triple[0] == 1 and self.triple[1] == 1:
                    patch = self.get_patch_hinge([xp,yp],0.03*self.Dim_X,self.sup_angle,'black')
                elif self.triple[0] == 0 and self.triple[1] == 1:
                    patch = self.get_patch_roller([xp,yp],0.03*self.Dim_X,self.sup_angle,'black')
                elif self.triple[0] == 1 and self.triple[1] == 0:
                    patch = self.get_patch_roller([xp,yp],0.03*self.Dim_X,90+self.sup_angle,'black')
#                 if self.triple[0] == 1 and self.triple[1] == 1 and self.triple[2] == 1:
#                     patch = self.get_patch_fixed([xp,yp],0.03*self.Dim_X,self.sup_angle,'black')
#                 elif self.triple[0] == 1 and self.triple[1] == 1 and self.triple[2] == 0:
#                     patch = self.get_patch_hinge([xp,yp],0.03*self.Dim_X,self.sup_angle,'black')
#                 elif self.triple[0] == 0 and self.triple[1] == 1 and self.triple[2] == 0:
#                     patch = self.get_patch_roller([xp,yp],0.03*self.Dim_X,self.sup_angle,'black')
#                 elif self.triple[0] == 1 and self.triple[1] == 0 and self.triple[2] == 0:
#                     patch = self.get_patch_roller([xp,yp],0.03*self.Dim_X,90+self.sup_angle,'black')
                
                   
                self.sup_patch = self.axes.add_patch(patch)
                self.canvas.draw()
        elif self.case == 5:
            dist = self.Dim_X
            for i in range(0,self.n_nodes):
                dx = event.xdata - self.NOD[i][1]
                dy = event.ydata - self.NOD[i][2]
                t_dist = (dx**2 + dy**2)**0.5
                if t_dist < dist:
                    dist = t_dist
                    idx = i
    
            if dist < 0.05*self.Dim_X:
                xp = self.NOD[idx][1]
                yp = self.NOD[idx][2]
                node_ID = self.get_node_ID(xp,yp)
                self.append_load(node_ID,self.F1)
                if hasattr(self, 'temp_F1'):
                    self.temp_F1.remove()
                    del self.temp_F1
                if hasattr(self, 'temp_F1M'):
                    self.temp_F1M.remove()
                    del self.temp_F1M
#                 self.canvas.draw()
                self.draw_figure()
            else:
                pass
            
        elif self.case == 6:
            
            if hasattr(self, 'cur_ele_x'):
                x, y = self.cur_ele_x, self.cur_ele_y
                element_ID = self.get_element_ID(self.get_node_ID(x[0],y[0]),self.get_node_ID(x[1],y[1]))
                if not self.Local:
                    self.CS = 1
                    [phi] = self.get_element_properties(element_ID-1,['phi'])
                    self.F21_final = np.cos(phi)*self.F2[0] - np.sin(phi)*self.F2[1]
                    self.F22_final = np.sin(phi)*self.F2[0] + np.cos(phi)*self.F2[1]
                else:
                    self.CS = 0
                    self.F21_final = self.F2[0]
                    self.F22_final = self.F2[1]
                
                self.append_F2(element_ID,[self.F21_final,self.F22_final,self.F2[2],self.F2[3]],self.CS)
                if hasattr(self, 'temp_F2'):
                    self.temp_F2.remove()
                    del self.temp_F2
                if hasattr(self, 'temp_F2M'):
                    self.temp_F2M.remove()
                    del self.temp_F2M
                self.draw_figure()
                
        elif self.case == 7:
            
            if hasattr(self, 'cur_ele_x'):
                x, y = self.cur_ele_x, self.cur_ele_y
                element_ID = self.get_element_ID(self.get_node_ID(x[0],y[0]),self.get_node_ID(x[1],y[1]))
                if not self.Local:
                    self.CS = 1
                    [phi] = self.get_element_properties(element_ID-1,['phi'])
                    self.F31_final = np.cos(phi)*self.F3[0] - np.sin(phi)*self.F3[1]
                    self.F32_final = np.sin(phi)*self.F3[0] + np.cos(phi)*self.F3[1]
                else:
                    self.CS = 0
                    self.F31_final = self.F3[0]
                    self.F32_final = self.F3[1]
                
                self.append_F3(element_ID,[self.F31_final,self.F32_final,self.F3[2]],self.CS)
                if hasattr(self, 'temp_F3'):
                    self.temp_F3.remove()
                    del self.temp_F3
                self.draw_figure()
                
        elif self.case == 8:
            
            if hasattr(self, 'cur_ele_x'):
                x, y = self.cur_ele_x, self.cur_ele_y
                element_ID = self.get_element_ID(self.get_node_ID(x[0],y[0]),self.get_node_ID(x[1],y[1]))
                
                self.append_F4(element_ID,self.F4)
                if hasattr(self, 'temp_F41'):
                    self.temp_F41.remove()
                    del self.temp_F41
                if hasattr(self, 'temp_F42'):
                    self.temp_F42.remove()
                    del self.temp_F42
                self.draw_figure()

        elif self.case == 9:
            dist = self.Dim_X
            for i in range(0,self.n_nodes):
                dx = event.xdata - self.NOD[i][1]
                dy = event.ydata - self.NOD[i][2]
                t_dist = (dx**2 + dy**2)**0.5
                if t_dist < dist:
                    dist = t_dist
                    idx = i
    
            if dist < 0.05*self.Dim_X:
                xp = self.NOD[idx][1]
                yp = self.NOD[idx][2]
                node_ID = self.get_node_ID(xp,yp)
                self.append_F5(node_ID,self.F5)
                if hasattr(self, 'temp_F51'):
                    self.temp_F51.pop(0).remove()
                    del self.temp_F51
                if hasattr(self, 'temp_F52'):
                    self.temp_F52.pop(0).remove()
                    del self.temp_F52
                self.draw_figure()
            else:
                pass



    def ClickStatOK(self, event):
        tpl = self.txt_ctrl_str.split(',')
        x = float(tpl[0])
        y = float(tpl[1])
        if self.case == 1:
            self.n1 = (x,y)
            self.n1_OK()
#             self.case = 2
        elif self.case == 2:
            self.n2 = (x,y)
            self.n2_OK()
        event.Skip()

    def ClickStatCANCEL(self, event):
        pass
        event.Skip()

    def ClickStatEDIT(self,event):
        if self.case == 31:
            dat = []
            sel_id = []
            for i in range(0,self.n_elements):
                if self.selected_el[i] == 1:
                    x1,y1 = self.NOD[self.ELE[i][1]-1][1:3]
                    x2,y2 = self.NOD[self.ELE[i][2]-1][1:3]
                    dat.append([x1,y1,x2,y2,self.ELE[i][3]])
                    sel_id.append(self.ELE[i][0])
               
            if len(dat) == 1: 
                dlg = EditElement(dat)
                dlg.CenterOnScreen()
                val = dlg.ShowModal()
                if val == wx.ID_OK:
                    xn1,yn1,xn2,yn2 = dlg.dat_out[0:4]
                    idx1 = self.get_node_ID(dat[0][0],dat[0][1])
                    idx2 = self.get_node_ID(dat[0][2],dat[0][3])
                    self.NOD[idx1-1][1], self.NOD[idx1-1][2] = xn1, yn1
                    self.NOD[idx2-1][1], self.NOD[idx2-1][2] = xn2, yn2
                    self.ELE[sel_id[0]-1][3] = dlg.dat_out[4]
                    self.gridNodes.SetCellValue(idx1-1,0,str(xn1))
                    self.gridNodes.SetCellValue(idx1-1,1,str(yn1))
                    self.gridNodes.SetCellValue(idx2-1,0,str(xn2))
                    self.gridNodes.SetCellValue(idx2-1,1,str(yn2))
                    self.gridElements.SetCellValue(sel_id[0]-1,2, str(int(dlg.dat_out[4])))
                   
                self.draw_figure()
                dlg.Destroy()
            elif len(dat) > 1:
                dlg = EditElement(dat)
                dlg.CenterOnScreen()
                val = dlg.ShowModal()
                if val == wx.ID_OK:
                    for i in range(0,len(sel_id)):
                        self.ELE[sel_id[i]-1][3] = dlg.dat_out
                        self.gridElements.SetCellValue(sel_id[i]-1,2, str(int(dlg.dat_out)))

                dlg.Destroy()
                
            for i in range(0,self.n_elements):
                self.selected_el[i] = 0
                
        elif self.case == 32:
            if self.sup_selected[0] == True:
                idx = self.get_support_ID(self.sup_selected[1])
                dlg = Support(self.BC[idx])
                dlg.CenterOnScreen()
                val = dlg.ShowModal()
                if val == wx.ID_OK:
                    self.BC[idx][1] = dlg.triple[0]
                    self.BC[idx][2] = dlg.triple[1]
                    self.BC[idx][3] = dlg.triple[2]
                    self.BC[idx][4] = dlg.sup_angle
                    self.gridBC.SetCellValue(idx,1,str(self.BC[idx][1]))
                    self.gridBC.SetCellValue(idx,2,str(self.BC[idx][2]))
                    self.gridBC.SetCellValue(idx,3,str(self.BC[idx][3]))
                    self.gridBC.SetCellValue(idx,4,str(self.BC[idx][4]))
                    self.sup_selected[0] = False
                    self.draw_figure()
                dlg.Destroy()

        elif self.case == 33:
            if self.f1_selected[0] == True:
                idx = self.f1_selected[1]
                dlg = F1(self.FOR1[idx],self.PATH)
                dlg.CenterOnScreen()
                val = dlg.ShowModal()
                if val == wx.ID_OK:
                    self.FOR1[idx][1] = dlg.dat_out[0]
                    self.FOR1[idx][2] = dlg.dat_out[1]
                    self.FOR1[idx][3] = dlg.dat_out[2]
                    self.f1_selected[0] = False
                    self.draw_figure()
                dlg.Destroy()
            
            if self.f2_selected[0] == True:
                idx = self.f2_selected[1]
                dlg = F2(self.FOR2[idx],self.PATH)
                dlg.CenterOnScreen()
                val = dlg.ShowModal()
                if val == wx.ID_OK:
                    self.FOR2[idx][1] = dlg.dat_out[0]
                    self.FOR2[idx][2] = dlg.dat_out[1]
                    self.FOR2[idx][3] = dlg.dat_out[2]
                    self.FOR2[idx][4] = dlg.dat_out[3]
                    self.f2_selected[0] = False
                    self.draw_figure()
                dlg.Destroy()
                
            if self.f3_selected[0] == True:
                idx = self.f3_selected[1]
                dlg = F3(self.FOR3[idx],self.PATH)
                dlg.CenterOnScreen()
                val = dlg.ShowModal()
                if val == wx.ID_OK:
                    self.FOR3[idx][1] = dlg.dat_out[0]
                    self.FOR3[idx][2] = dlg.dat_out[1]
                    self.FOR3[idx][3] = dlg.dat_out[2]
                    self.f3_selected[0] = False
                    self.draw_figure()
                dlg.Destroy()

        
        event.Skip()

    def ClickStatDELETE(self,event):
        if self.case == 31:
            Dead_el = []
            for i in range(0,self.n_elements):
                if self.selected_el[i] == 1:
                    Dead_el.append(i)
            
            for i in range(0,len(Dead_el)):
                self.Ele_to_del = Dead_el[i]
                self.OnDelEle(0)
                for j in range(i+1,len(Dead_el)):
                    Dead_el[j] = Dead_el[j] - 1
            self.draw_figure()
        elif self.case == 32:
            if self.sup_selected[0] == True:
                self.sup_selected[0] = False
                idx = self.get_support_ID(self.sup_selected[1])
                self.BC_to_del = idx
                self.OnDelBC(0)
                self.draw_figure()
        elif self.case == 33:
            if self.f1_selected[0] == True:
                self.f1_selected[0] = False
                idx = self.f1_selected[1]
                del self.FOR1[idx]
                self.draw_figure()

            if self.f2_selected[0] == True:
                self.f2_selected[0] = False
                idx = self.f2_selected[1]
                del self.FOR2[idx]
                del self.f2_CS[idx]
                self.draw_figure()
                
            if self.f3_selected[0] == True:
                self.f3_selected[0] = False
                idx = self.f3_selected[1]
                del self.FOR3[idx]
                del self.f3_CS[idx]
                self.draw_figure()
                
        event.Skip()

    def n1_OK(self):
        self.case = 2
        self.update_status_bar()
        exists = self.node_exists(self.n1)
        if exists[0] == False:
            self.append_node(self.n1)
            self.Draw_node(self.n1, self.n_nodes)
            self.id_n1 = self.n_nodes
        else:
            self.id_n1 = int(exists[1])
            
    def n2_OK(self):        
        self.case = 1
        self.update_status_bar()
        exists = self.node_exists(self.n2)
        if exists[0] == False:
            self.append_node(self.n2)
            self.Draw_node(self.n2, self.n_nodes)
            self.id_n2 = self.n_nodes
        else:
            self.id_n2 = int(exists[1])
            
        self.append_element(self.id_n1,self.id_n2)
        self.Draw_element(self.n1,self.n2,self.n_elements)

    def append_node(self,xy):
        self.n_nodes = self.n_nodes + 1
        self.gridNodes.AppendRows(1, updateLabels = True)
        self.NOD.append([self.n_nodes ,xy[0],xy[1]])
        self.gridNodes.SetCellValue(self.n_nodes-1,0,str(xy[0]))
        self.gridNodes.SetCellValue(self.n_nodes-1,1,str(xy[1]))   
        
        
    def append_element(self,j,k):
        self.n_elements = self.n_elements + 1
        self.selected_el.append(0)
        self.gridElements.AppendRows(1, updateLabels = True)
        self.ELE.append([self.n_elements ,j,k,1])
        self.gridElements.SetCellValue(self.n_elements-1,0,str(j))
        self.gridElements.SetCellValue(self.n_elements-1,1,str(k))
        self.gridElements.SetCellValue(self.n_elements-1,2,str(1))  

    def append_support(self,idx,triple,angle):
        self.n_supports = self.n_supports + 1
        self.gridBC.AppendRows(1, updateLabels = True)
        R1 = self.gridBC.GetNumberRows()
        self.gridBC.SetCellEditor(R1-1, 1, self.choice_editor)
        self.gridBC.SetCellEditor(R1-1, 2, self.choice_editor)
        self.gridBC.SetCellEditor(R1-1, 3, self.choice_editor)
#         self.BC.append([idx, triple[0], triple[1], triple[2], angle])        
        self.BC.append([idx, triple[0], triple[1], angle])        
        self.gridBC.SetCellValue(self.n_supports-1,0,str(idx))
        self.gridBC.SetCellValue(self.n_supports-1,1,str(triple[0]))   
        self.gridBC.SetCellValue(self.n_supports-1,2,str(triple[1]))
#         self.gridBC.SetCellValue(self.n_supports-1,3,str(triple[2]))
        self.gridBC.SetCellValue(self.n_supports-1,3,str(angle))

    def append_load(self,idx,xy):
        self.n_loads = self.n_loads + 1
        self.gridForces.AppendRows(1, updateLabels = True)
        self.FOR1.append([idx ,xy[0],xy[1]])
        self.gridForces.SetCellValue(self.n_loads-1,0,str(idx))
        self.gridForces.SetCellValue(self.n_loads-1,1,str(xy[0]))
        self.gridForces.SetCellValue(self.n_loads-1,2,str(xy[1]))
#         print self.FOR1 

    def append_F1(self,idx,F1):
#         self.FOR1.append([idx, F1[0], F1[1], F1[2]])   
        self.FOR1.append([idx, F1[0], F1[1]])   
        
#     def append_F2(self,idx,F2,CS):
#         self.f2_CS.append(CS)
#         self.FOR2.append([idx, F2[0], F2[1], F2[2], F2[3]])   
#         
#     def append_F3(self,idx,F3,CS):
#         self.f3_CS.append(CS)
#         self.FOR3.append([idx, F3[0], F3[1], F3[2]])      
# 
#     def append_F4(self,idx,F4):
#         self.FOR4.append([idx, F4[0], F4[1], F4[2]])
#         
#     def append_F5(self,idx,F5):
#         self.FOR5.append([idx, F5[0], F5[1], F5[2]])
#         
#     def append_F6(self,idx,F6):
#         self.FOR6.append([idx, F6[0], F6[1], F6[2]])

    def node_exists(self,xy):
        val = False
        idx = 0
        for i in range(0,self.n_nodes):
            if self.NOD[i][1] == xy[0] and self.NOD[i][2] == xy[1]:
                val = True
                idx = self.NOD[i][0]

        return val,idx

    def uncheck_tools(self,tb,ID_cur):
        for i in self.IDs:
            if i != ID_cur:
                tb.ToggleTool(i,False)
    
    def update_status_bar(self):
        if self.case == 0:
            self.add_txt_ctrl('')
        elif self.case == 1:
            self.add_txt_ctrl('Set starting node coordinates [X,Y] : ')
        elif self.case == 2:
            self.add_txt_ctrl('Set ending node coordinates [X,Y] : ')
        elif self.case == 4:
            self.add_txt_ctrl('Pick a Node')
        elif self.case == 5:
            self.add_txt_ctrl('Pick a Node')
        elif self.case == 6:
            self.add_txt_ctrl('Pick an Element')
        elif self.case == 7:
            self.add_txt_ctrl('Pick an Element')
        elif self.case == 8:
            self.add_txt_ctrl('Pick an Element')
        elif self.case == 9:
            self.add_txt_ctrl('Pick a Node')        
        elif self.case == 31 or self.case == 32 or self.case == 33:
            self.add_txt_ctrl('')

    def add_txt_ctrl(self,msg):
        self.statusbarText = wx.StaticText(self.statusbar, -1, msg)
        self.statusbar.AddWidget(self.statusbarText, pos = 1)
        self.txt_ctrl_str = '0,0'
        self.TC_n1 = wx.TextCtrl(self.statusbar, -1, self.txt_ctrl_str, size=(50, -1))
        self.statusbar.AddWidget(self.TC_n1, ESB.ESB_EXACT_FIT ,pos = 2)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.TC_n1)        

    def add_stat_buttons(self):
        img = wx.ART_ERROR
        bmp = wx.ArtProvider_GetBitmap(img, wx.ART_TOOLBAR, (16,16))
        
        stat_OK = wx.Button(self.statusbar, -1, "OK", size = (30,-1))
        stat_OK.Bind(wx.EVT_LEFT_UP, self.ClickStatOK)
        stat_CANCEL = wx.StaticBitmap(self.statusbar, -1, bmp)
        stat_CANCEL.Bind(wx.EVT_LEFT_UP, self.ClickStatCANCEL)
        
        self.statusbar.AddWidget(stat_OK, pos = 3) 
        self.statusbar.AddWidget(stat_CANCEL, pos = 4)
        
    def add_stat_buttons2(self):
        stat_EDIT = wx.Button(self.statusbar, -1, "Edit", size = (100,-1))
        stat_EDIT.Bind(wx.EVT_LEFT_UP, self.ClickStatEDIT)
        stat_DELETE = wx.Button(self.statusbar, -1, "Delete", size = (100,-1))
        stat_DELETE.Bind(wx.EVT_LEFT_UP, self.ClickStatDELETE)
        
        self.statusbar.AddWidget(stat_EDIT, pos = 1) 
        self.statusbar.AddWidget(stat_DELETE, pos = 2)

    def remove_stat_buttons(self):
        f1 = wx.StaticText(self.statusbar, -1, '', size=(50, -1))
        f2 = wx.StaticText(self.statusbar, -1, '', size=(50, -1))
        f3 = wx.StaticText(self.statusbar, -1, '', size=(50, -1))
        self.statusbar.AddWidget(f1, ESB.ESB_EXACT_FIT ,pos = 2)
        self.statusbar.AddWidget(f2, ESB.ESB_EXACT_FIT ,pos = 3)
        self.statusbar.AddWidget(f3, ESB.ESB_EXACT_FIT ,pos = 4)

    def Set_State(self,state):
        if state == 0 or state == 3:
            self.case = state
            self.update_status_bar()
            self.remove_stat_buttons()
        elif state == 1 or state == 2:
            self.case = state
            self.update_status_bar()
            self.add_stat_buttons()
        elif state == 31 or state == 32 or state == 33:
            self.case = state
            self.update_status_bar()
            if np.sum(self.selected_el) != 0 or \
               self.sup_selected[0] == True or  \
               self.f1_selected[0]  == True or  \
               self.f2_selected[0]  == True or  \
               self.f3_selected[0]  == True:
                self.add_stat_buttons2()
            else:
                self.remove_stat_buttons()
        else:
            self.case = state
            self.update_status_bar()
            self.remove_stat_buttons()

    def EvtText(self, event):
        self.txt_ctrl_str = event.GetString()

    def Draw_node(self,xy,idx):
#         ND = self.axes.scatter(xy[0], xy[1], color = self.col1, s=5)
        dist = float(self.Dim_X)/float(60)
        if self.labelN == True:
            self.axes.text(xy[0]+dist,xy[1]+dist, str(int(idx)), size='small',color=self.col1)
        self.canvas.draw()
        
    def Draw_element(self,xy1,xy2,idx):
        self.axes.plot([xy1[0],xy2[0]], [xy1[1],xy2[1]], color = self.col1,linewidth=1.5,picker = 0.25*self.Dim_X)
        dist = float(self.Dim_X)/float(60)
        p = ((xy1[0]+xy2[0])/2-2*dist,(xy1[1]+xy2[1])/2+dist)
        if self.labelE == True:
            self.axes.text(p[0],p[1],'E'+str(int(idx)), size='small',color=self.col1,alpha=0.5)
        self.canvas.draw()
        
    def create_status_bar(self):
        self.statusbar = ESB.EnhancedStatusBar(self, -1)
        self.statusbar.SetSize((-1, 30))
        self.statusbar.SetFieldsCount(6)
        self.statusbar.SetBackgroundColour(wx.LIGHT_GREY)
        self.SetStatusBar(self.statusbar)        
        self.statusbar.SetStatusWidths([150, 250, 100,30,30, -1])

    def on_text_enter(self, event):
        self.draw_figure()
        
    def on_cb_grid(self, event):
        self.axes.grid(self.m_grid.IsChecked())
        self.canvas.draw()

    def Properties(self, parent):
        self.quoteS = wx.StaticText(self.PropertieTab, label="Section properties.")
        
        self.gridSections = wx.grid.Grid(self.PropertieTab, size=(280,160))
        
        self.gridSections.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.SGridUpdate)
        self.gridSections.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.SGridRight)
        self.gridSections.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.SGridRight)
        self.gridSections.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowS)
        self.gridSections.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowS)
        
        self.gridSections.CreateGrid(1,2)
        self.gridSections.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )

        self.gridSections.SetColLabelValue(0, 'A')
#         self.gridSections.SetColLabelValue(1, 'I')
#         self.gridSections.SetColLabelValue(2, 'h')
        self.gridSections.SetColLabelValue(1, 'Material ID')
        self.gridSections.SetRowLabelSize(100)
        self.gridSections.SetColSize(1, 100)

        self.gridSections.SetCellValue(0,0,str(self.SEC[0][1]))
#         self.gridSections.SetCellValue(0,1,str(self.SEC[0][2]))
#         self.gridSections.SetCellValue(0,2,str(self.SEC[0][3]))
        self.gridSections.SetCellValue(0,1,str(int(self.SEC[0][2])))
        
        corn_S = self.gridSections.GetGridCornerLabelWindow()
        S_ID = wx.StaticText(corn_S, label="Section ID",pos=(9,8))
        font = wx.Font(9, wx.DEFAULT, wx.NORMAL, wx.BOLD)
        S_ID.SetFont(font)

        # A button
        
        self.hbox1 = wx.BoxSizer(wx.HORIZONTAL)
        self.buttonS = wx.Button(self.PropertieTab, label="Add")
        self.Bind(wx.EVT_BUTTON, self.OnClickS, self.buttonS)
        
        self.buttonDelS = wx.Button(self.PropertieTab, label="Delete")
        self.Bind(wx.EVT_BUTTON, self.ClickDelSec, self.buttonDelS)
        
        flags =  wx.ALL | wx.ALIGN_CENTER_VERTICAL
        self.hbox1.Add(self.quoteS, 0, border=3, flag=flags)
        self.hbox1.AddStretchSpacer(1)
        self.hbox1.Add(self.buttonS, 0, border=3, flag=flags)
        self.hbox1.Add(self.buttonDelS, 0, border=3, flag=flags)
         
        self.quoteM = wx.StaticText(self.PropertieTab, label="Material properties.")
        
        self.gridMaterials = wx.grid.Grid(self.PropertieTab, size=(260,160))
        
        self.gridMaterials.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.MGridUpdate)
        self.gridMaterials.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.MGridRight)
        self.gridMaterials.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.MGridRight)
        self.gridMaterials.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowM)
        self.gridMaterials.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowM)
        
        self.gridMaterials.CreateGrid(1,2)
        self.gridMaterials.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
        
        self.gridMaterials.SetColLabelValue(0, 'Density')
        self.gridMaterials.SetColLabelValue(1, 'E')
#         self.gridMaterials.SetColLabelValue(2, 'a')
        self.gridMaterials.SetRowLabelSize(100)
        
        self.gridMaterials.SetCellValue(0,0,str(self.MAT[0][1]))
        self.gridMaterials.SetCellValue(0,1,str(self.MAT[0][2]))
#         self.gridMaterials.SetCellValue(0,2,str(self.MAT[0][3]))
        
        corn_M = self.gridMaterials.GetGridCornerLabelWindow()
        M_ID = wx.StaticText(corn_M, label="Material ID",pos=(9,8))
        font = wx.Font(9, wx.DEFAULT, wx.NORMAL, wx.BOLD)
        M_ID.SetFont(font)        
        
#         for i in range(1,6):
#             self.gridSections.SetColSize(i, 50)
         
        # A button
        self.hbox2 = wx.BoxSizer(wx.HORIZONTAL)
        self.buttonM = wx.Button(self.PropertieTab, label="Add")
        self.Bind(wx.EVT_BUTTON, self.OnClickM, self.buttonM)
        
        self.buttonDelM = wx.Button(self.PropertieTab, label="Delete")
        self.Bind(wx.EVT_BUTTON, self.ClickDelMat, self.buttonDelM)
        
        flags =  wx.ALL | wx.ALIGN_CENTER_VERTICAL
        self.hbox2.Add(self.quoteM, 0, border=3, flag= flags )
        self.hbox2.AddStretchSpacer(1)
        self.hbox2.Add(self.buttonM, 0, border=3, flag= flags)
        self.hbox2.Add(self.buttonDelM, 0, border=3, flag= flags )

        
        self.buttonDoneP = wx.Button(self.PropertieTab, label="Done")
        self.Bind(wx.EVT_BUTTON, self.OnClickDoneP, self.buttonDoneP)
           
        self.vbox2S = wx.BoxSizer(wx.VERTICAL)
        flags = wx.ALIGN_LEFT | wx.ALL | wx.ALIGN_CENTER_VERTICAL
        self.vbox2S.Add(self.hbox2, 0, border=3, flag=wx.EXPAND)
        self.vbox2S.Add(self.gridMaterials, 0, border=3, flag=flags)
#         self.vbox2S.Add(self.hbox2, 0, border=3, flag=wx.ALIGN_LEFT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        
        self.vbox2S.AddSpacer(5)
        line = wx.StaticLine(self.PropertieTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        self.vbox2S.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        self.vbox2S.AddSpacer(5)
        
        self.vbox2S.Add(self.hbox1, 0, border=3, flag=wx.EXPAND)
        self.vbox2S.Add(self.gridSections, 0, border=3, flag=flags)
#         self.vbox2S.Add(self.hbox1, 0, border=3, flag=wx.ALIGN_RIGHT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        
        self.vbox2S.AddSpacer(5)
        line = wx.StaticLine(self.PropertieTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        self.vbox2S.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        self.vbox2S.AddSpacer(5)
        
        self.vbox2S.Add(self.buttonDoneP, 0, border=3, flag=wx.ALIGN_RIGHT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)

    
    def Geometry(self, parent):
        self.labelN = True
        
        self.quoteN = wx.StaticText(self.GeometryTab, label="Nodal coordinates.")
        
        self.gridNodes = wx.grid.Grid(self.GeometryTab, size=(270,160))
        
        self.gridNodes.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.NGridUpdate)
        self.gridNodes.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.NGridRight)
        self.gridNodes.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.NGridRight)
        self.gridNodes.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowN)
        self.gridNodes.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowN)
        
        self.gridNodes.CreateGrid(0,2)
        self.gridNodes.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
        self.gridNodes.SetRowLabelSize(80)
        self.gridNodes.SetColLabelValue(0, 'X')
        self.gridNodes.SetColLabelValue(1, 'Y')
        
        corn_N = self.gridNodes.GetGridCornerLabelWindow()
        N_ID = wx.StaticText(corn_N, label="Node ID",pos=(10,8))
        font = wx.Font(9, wx.DEFAULT, wx.NORMAL, wx.BOLD)
        N_ID.SetFont(font)
        # A button
        self.buttonN = wx.Button(self.GeometryTab, label="Add")
        self.Bind(wx.EVT_BUTTON, self.OnClickN, self.buttonN)
        
        self.buttonDelN = wx.Button(self.GeometryTab, label="Delete")
        self.Bind(wx.EVT_BUTTON, self.ClickDelNod, self.buttonDelN)

        self.hbox2N = wx.BoxSizer(wx.HORIZONTAL)
        flags =  wx.ALL | wx.ALIGN_CENTER_VERTICAL
        self.hbox2N.Add(self.quoteN, 0, border=3, flag= flags )
        self.hbox2N.AddStretchSpacer(1)
        self.hbox2N.Add(self.buttonN, 0, border=3, flag= flags)
        self.hbox2N.Add(self.buttonDelN, 0, border=3, flag= flags )

        self.labelE = True
        
        self.quoteE = wx.StaticText(self.GeometryTab, label="Element definition.")
        
        self.gridElements = wx.grid.Grid(self.GeometryTab, size=(360,160))
        
        self.gridElements.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.EGridUpdate)
        self.gridElements.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.EGridRight)
        self.gridElements.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.EGridRight)
        self.gridElements.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowE)
        self.gridElements.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowE)      
        
        self.gridElements.CreateGrid(0,3)
        self.gridElements.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
        self.gridElements.SetRowLabelSize(100)
        self.gridElements.SetColLabelValue(0, 'N1')
        self.gridElements.SetColLabelValue(1, 'N2')
        self.gridElements.SetColLabelValue(2, 'Section ID')
        
        corn_E = self.gridElements.GetGridCornerLabelWindow()
        E_ID = wx.StaticText(corn_E, label="Element ID",pos=(9,8))
        font = wx.Font(9, wx.DEFAULT, wx.NORMAL, wx.BOLD)
        E_ID.SetFont(font)
         
        # A button
        self.buttonE = wx.Button(self.GeometryTab, label="Add")
        self.Bind(wx.EVT_BUTTON, self.OnClickE, self.buttonE)

        self.buttonDelE = wx.Button(self.GeometryTab, label="Delete")
        self.Bind(wx.EVT_BUTTON, self.ClickDelEle, self.buttonDelE)

        self.buttonDoneG = wx.Button(self.GeometryTab, label="Done")
        self.Bind(wx.EVT_BUTTON, self.OnClickDoneG, self.buttonDoneG)
           
        self.hbox2E = wx.BoxSizer(wx.HORIZONTAL)
        flags =  wx.ALL | wx.ALIGN_CENTER_VERTICAL
        self.hbox2E.Add(self.quoteE, 0, border=3, flag= flags )
        self.hbox2E.AddStretchSpacer(1)
        self.hbox2E.Add(self.buttonE, 0, border=3, flag= flags )
        self.hbox2E.Add(self.buttonDelE, 0, border=3, flag= flags )

        self.vbox2G = wx.BoxSizer(wx.VERTICAL)
        flags = wx.ALIGN_LEFT | wx.ALL | wx.ALIGN_CENTER_VERTICAL
        self.vbox2G.Add(self.hbox2N, 0, border=3, flag=wx.EXPAND)
        self.vbox2G.Add(self.gridNodes, 0, border=3, flag=flags)

        self.vbox2G.AddSpacer(5)
        line = wx.StaticLine(self.GeometryTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        self.vbox2G.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        self.vbox2G.AddSpacer(5)         
        
        self.vbox2G.Add(self.hbox2E, 0, border=3, flag=wx.EXPAND)
        self.vbox2G.Add(self.gridElements, 0, border=3, flag=flags)
        
        self.vbox2G.AddSpacer(5)
        line = wx.StaticLine(self.GeometryTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        self.vbox2G.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        self.vbox2G.AddSpacer(5)        
        
        self.vbox2G.Add(self.buttonDoneG, 0, border=3, flag=wx.ALIGN_RIGHT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)

    def BCs_Loads(self, parent):

        self.quoteB = wx.StaticText(self.BCTab, label="Support type on corresponding node.")
        
        self.gridBC = wx.grid.Grid(self.BCTab, size=(343,160))
        
        self.gridBC.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.BCGridUpdate)
        self.gridBC.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.BCGridRight)
        self.gridBC.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.BCGridRight)
        self.gridBC.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowBC)
        self.gridBC.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowBC)   
        
        self.gridBC.CreateGrid(0,4)
        self.gridBC.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
        self.gridBC.SetRowLabelSize(1)
        self.gridBC.SetColLabelValue(0, 'Node ID')
        self.gridBC.SetColLabelValue(1, 'X')
        self.gridBC.SetColLabelValue(2, 'Y')
        self.gridBC.SetColLabelValue(3, 'angle')
        self.gridBC.SetColSize(1,50)
        self.gridBC.SetColSize(2,50)
        self.gridBC.SetColSize(3,50)
        
        choices_list = ['0','1']
        self.choice_editor = wx.grid.GridCellChoiceEditor(choices_list, False) 
#         self.gridBC.SetCellEditor(0, 1, self.choice_editor)
#         self.gridBC.SetCellEditor(0, 2, self.choice_editor)
#         self.gridBC.SetCellEditor(0, 3, self.choice_editor)
        
                # A button
        self.buttonBC = wx.Button(self.BCTab, label="Add")
        self.Bind(wx.EVT_BUTTON, self.OnClickBC, self.buttonBC)
        
        self.buttonDelBC = wx.Button(self.BCTab, label="Delete")
        self.Bind(wx.EVT_BUTTON, self.ClickDelBC, self.buttonDelBC)

        self.hbox2BC = wx.BoxSizer(wx.HORIZONTAL)
        flags =  wx.ALL | wx.ALIGN_CENTER_VERTICAL
        self.hbox2BC.Add(self.quoteB, 0, border=3, flag= flags )
        self.hbox2BC.AddStretchSpacer(1)
        self.hbox2BC.Add(self.buttonBC, 0, border=3, flag= flags)
        self.hbox2BC.Add(self.buttonDelBC, 0, border=3, flag= flags )

        self.buttonDoneBC = wx.Button(self.BCTab, label="Done")
        self.Bind(wx.EVT_BUTTON, self.OnClickDoneBC, self.buttonDoneBC)
        
        self.vbox2B = wx.BoxSizer(wx.VERTICAL)
        flags = wx.ALIGN_LEFT | wx.ALL | wx.ALIGN_CENTER_VERTICAL
        self.vbox2B.AddSpacer(10)
        self.vbox2B.Add(self.hbox2BC, 0, border=3, flag=wx.EXPAND)
        self.vbox2B.Add(self.gridBC, 0, border=3, flag=flags)
        
        self.vbox2B.AddSpacer(5)
        line = wx.StaticLine(self.BCTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        self.vbox2B.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        self.vbox2B.AddSpacer(5) 
        
        

        self.show_F1 = True
        self.label_F1 = True

         
        self.quoteL = wx.StaticText(self.BCTab, label="Forces on nodes.")
         
        self.gridForces = wx.grid.Grid(self.BCTab, size=(250,160))
         
        self.gridForces.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.LGridUpdate)
        self.gridForces.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.LGridRight)
        self.gridForces.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.LGridRight)
        self.gridForces.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowL)
        self.gridForces.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowL)
          
        self.gridForces.CreateGrid(0,3)
        self.gridForces.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
        self.gridForces.SetRowLabelSize(1)
        
        self.gridForces.SetColLabelValue(0, 'Node ID')
        self.gridForces.SetColLabelValue(1, 'X')
        self.gridForces.SetColLabelValue(2, 'Y')
          
        corn_L = self.gridForces.GetGridCornerLabelWindow()
        L_ID = wx.StaticText(corn_L, label="Node ID",pos=(10,8))
        font = wx.Font(9, wx.DEFAULT, wx.NORMAL, wx.BOLD)
        L_ID.SetFont(font)

        # A button
        self.buttonL = wx.Button(self.BCTab, label="Add")
        self.Bind(wx.EVT_BUTTON, self.OnClickL, self.buttonL)
         
        self.buttonDelL = wx.Button(self.BCTab, label="Delete")
        self.Bind(wx.EVT_BUTTON, self.ClickDelLoad, self.buttonDelL)
 
        self.hbox2L = wx.BoxSizer(wx.HORIZONTAL)
        flags =  wx.ALL | wx.ALIGN_CENTER_VERTICAL
        self.hbox2L.Add(self.quoteL, 0, border=3, flag= flags )
        self.hbox2L.AddStretchSpacer(1)
        self.hbox2L.Add(self.buttonL, 0, border=3, flag= flags)
        self.hbox2L.Add(self.buttonDelL, 0, border=3, flag= flags )

        self.vbox2B.Add(self.hbox2L, 0, border=3, flag=wx.EXPAND)
        self.vbox2B.Add(self.gridForces, 0, border=3, flag=flags)

        self.vbox2B.Add(self.buttonDoneBC, 0, border=3, flag=wx.ALIGN_RIGHT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)

#     def Forces(self, parent):
#         self.show_F1 = True
#         self.show_F2 = True
#         self.show_F3 = True
#         self.show_F4 = True
#         self.show_F5 = True
#         self.show_F6 = True
#         
#         self.label_F1 = True
#         self.label_F2 = True
#         self.label_F3 = True
#         self.label_F4 = True
#         self.label_F5 = True
#         self.label_F6 = True
# 
#         self.quoteFN = wx.StaticText(self.ForceTab, label="Loads on nodes.")
#         self.quoteFE = wx.StaticText(self.ForceTab, label="Loads on elements.")
#         
# 
#         # A button
#         self.buttonF1 = wx.Button(self.ForceTab,-1,'',size=(35,35))
#         self.buttonF1.SetToolTipString("Nodal Forces")
#         self.Bind(wx.EVT_BUTTON, self.OnClickF1, self.buttonF1)
# #         self.buttonF1.SetBitmap(wx.Image(self.PATH + '\icons\\nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap()) 
#         self.buttonF1.SetBitmap(wx.Image(self.PATH + '/icons/nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap()) 
#         
#         self.buttonF2 = wx.Button(self.ForceTab,-1,'',size=(35,35))
#         self.buttonF2.SetToolTipString("Element Forces")
#         self.Bind(wx.EVT_BUTTON, self.OnClickF2, self.buttonF2)
# #         self.buttonF2.SetBitmap(wx.Image(self.PATH + '\icons\\ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap()) 
#         self.buttonF2.SetBitmap(wx.Image(self.PATH + '/icons/ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
# 
#         self.buttonF3 = wx.Button(self.ForceTab,-1,'',size=(35,35))
#         self.buttonF3.SetToolTipString("Distributed Loads")
#         self.Bind(wx.EVT_BUTTON, self.OnClickF3, self.buttonF3)
# #         self.buttonF3.SetBitmap(wx.Image(self.PATH + '\icons\\dl.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap()) 
#         self.buttonF3.SetBitmap(wx.Image(self.PATH + '/icons/dl.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap()) 
#         
#         self.buttonF4 = wx.Button(self.ForceTab,-1,'',size=(35,35))
#         self.buttonF4.SetToolTipString("Temperature Loads")
#         self.Bind(wx.EVT_BUTTON, self.OnClickF4, self.buttonF4)
# #         self.buttonF4.SetBitmap(wx.Image(self.PATH + '\icons\\tl.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap()) 
#         self.buttonF4.SetBitmap(wx.Image(self.PATH + '/icons/tl.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#         
#         self.buttonF5 = wx.Button(self.ForceTab,-1,'',size=(35,35))
#         self.buttonF5.SetToolTipString("Support Displacements")
#         self.Bind(wx.EVT_BUTTON, self.OnClickF5, self.buttonF5)
# #         self.buttonF5.SetBitmap(wx.Image(self.PATH + '\icons\\sd.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap()) 
#         self.buttonF5.SetBitmap(wx.Image(self.PATH + '/icons/sd.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
# 
#         self.buttonF6 = wx.Button(self.ForceTab,-1,'',size=(35,35))
#         self.buttonF6.SetToolTipString("Structural Defects")
#         self.Bind(wx.EVT_BUTTON, self.OnClickF6, self.buttonF6)
# #         self.buttonF6.SetBitmap(wx.Image(self.PATH + '\icons\\std.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#         self.buttonF6.SetBitmap(wx.Image(self.PATH + '/icons/std.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
# 
#         self.buttonDoneF = wx.Button(self.ForceTab, label="Done")
#         self.Bind(wx.EVT_BUTTON, self.OnClickDoneF, self.buttonDoneF)
# 
#         self.hbox1 = wx.BoxSizer(wx.HORIZONTAL)
#         
#         flags = wx.GROW|wx.RIGHT|wx.EXPAND
#         self.hbox1.Add( self.quoteFN, 0, border=36, flag=flags )
#         self.hbox1.Add( self.buttonF1, 0, border=20, flag=flags )
#         self.hbox1.Add( self.buttonF5, 0, border=20, flag=flags )
#         self.hbox1.Add( self.buttonF6, 0, border=20, flag=flags )
#         
#         self.hbox2 = wx.BoxSizer(wx.HORIZONTAL)
#         
#         self.hbox2.Add( self.quoteFE, 0, border=20, flag=flags )
#         self.hbox2.Add( self.buttonF2, 0, border=20, flag=flags )
#         self.hbox2.Add( self.buttonF3, 0, border=20, flag=flags )
#         self.hbox2.Add( self.buttonF4, 0, border=20, flag=flags )
# 
#         self.vbox2F = wx.BoxSizer(wx.VERTICAL)
#         flags = wx.ALIGN_LEFT | wx.ALL | wx.ALIGN_CENTER_VERTICAL
#         self.vbox2F.AddSpacer(10)
#         self.vbox2F.Add(self.hbox1, 0, border=3, flag=flags)
# 
#         self.vbox2F.AddSpacer(5)
#         line = wx.StaticLine(self.ForceTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#         self.vbox2F.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#         self.vbox2F.AddSpacer(5) 
# 
#         self.vbox2F.Add(self.hbox2, 0, border=3, flag=flags)
#         
#         self.vbox2F.AddSpacer(5)
#         line = wx.StaticLine(self.ForceTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#         self.vbox2F.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#         self.vbox2F.AddSpacer(5) 
# 
#         self.vbox2F.Add(self.buttonDoneF, 0, border=3, flag=wx.ALIGN_RIGHT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)

    def ActiveRowM(self,event):
        self.active_rowM = event.GetRow()
        event.Skip()

    def ClickDelMat(self,event):
        self.Mat_to_del = self.active_rowM
        self.OnDelMat(0)

    def ActiveRowS(self,event):
        self.active_rowS = event.GetRow()
        event.Skip()

    def ClickDelSec(self,event):
        self.Sec_to_del = self.active_rowS
        self.OnDelSec(0)
        
    def ActiveRowN(self,event):
        self.active_rowN = event.GetRow()
        event.Skip()

    def ClickDelNod(self,event):
        self.Nod_to_del = self.active_rowN
        self.OnDelNod(0)

    def ActiveRowE(self,event):
        self.active_rowE = event.GetRow()
        event.Skip()

    def ClickDelEle(self,event):
        self.Ele_to_del = self.active_rowE
        self.OnDelEle(0)
 
    def ActiveRowBC(self,event):
        self.active_rowBC = event.GetRow()
        event.Skip()

    def ClickDelBC(self,event):
        self.BC_to_del = self.active_rowBC
        self.OnDelBC(0)

    def ActiveRowL(self,event):
        self.active_rowL = event.GetRow()
        event.Skip()

    def ClickDelLoad(self,event):
        self.For_to_del = self.active_rowL
        self.OnDelFor(0)
        
    def OnClickS(self,event):
        self.gridSections.AppendRows(1, updateLabels = True)
        R = len(self.SEC)
        self.SEC.append([R+1 ,0.,0., 0., 0.])
        self.active_rowS = R-1
        
    def OnClickM(self,event):
        self.gridMaterials.AppendRows(1, updateLabels = True)
        R = len(self.MAT)
        self.MAT.append([R+1 ,0.,0., 0.])
        self.active_rowM = R-1

    def OnClickN(self,event):
        self.append_node((0.0,0.0))
#         self.Draw_node((0.0,0.0), self.n_nodes)

    def OnClickE(self,event):
        self.append_element(0,0)
#         self.Draw_element(0,0, self.n_elements)

    def OnClickBC(self,event):
        self.append_support(1, [0,0,0], 0)

    def OnClickL(self,event):
        self.append_load(1,[0.0,0.0])

    def OnClickF1(self,event):
        dlg = NodalForce(self.FOR1,self.PATH)
        dlg.CenterOnScreen()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.FOR1 = dlg.FOR
            self.draw_figure()
        else:
            pass
        dlg.Destroy()

#     def OnClickF2(self,event):
#         dlg = ElementForce(self.FOR2,self.PATH)
#         dlg.CenterOnScreen()
#         val = dlg.ShowModal()
#         if val == wx.ID_OK:
#             self.FOR2 = dlg.FOR
#             self.draw_figure()
#         else:
#             pass
#         dlg.Destroy()
# 
#     def OnClickF3(self,event):
#         dlg = DistributedLoad(self.FOR3,self.PATH)
#         dlg.CenterOnScreen()
#         val = dlg.ShowModal()
#         if val == wx.ID_OK:
#             self.FOR3 = dlg.FOR
#             self.draw_figure()
#         else:
#             pass
#         dlg.Destroy()
#         
#     def OnClickF4(self,event):
#         dlg = TemperatureLoad(self.FOR4,self.PATH)
#         dlg.CenterOnScreen()
#         val = dlg.ShowModal()
#         if val == wx.ID_OK:
#             self.FOR4 = dlg.FOR
#             self.draw_figure()
#         else:
#             pass
#         dlg.Destroy()
#         
#     def OnClickF5(self,event):
#         dlg = SupportDisplacement(self.FOR5,self.PATH)
#         dlg.CenterOnScreen()
#         val = dlg.ShowModal()
#         if val == wx.ID_OK:
#             self.FOR5 = dlg.FOR
#             self.draw_figure()
#         else:
#             pass
#         dlg.Destroy()
#     
#     def OnClickF6(self,event):
#         dlg = StructuralDefect(self.FOR6,self.PATH)
#         dlg.CenterOnScreen()
#         val = dlg.ShowModal()
#         if val == wx.ID_OK:
#             self.FOR6 = dlg.FOR
#             self.draw_figure()
#         else:
#             pass
#         dlg.Destroy()

    def SGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        Value = float(self.gridSections.GetCellValue(R, C))
        self.SEC[R][C+1] = Value
        self.draw_figure()
        
    def MGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        Value = float(self.gridMaterials.GetCellValue(R, C))
        self.MAT[R][C+1] = Value
        self.draw_figure()

    def NGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        Value = float(self.gridNodes.GetCellValue(R, C))
        self.NOD[R][C+1] = Value

    def LGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        Value = float(self.gridForces.GetCellValue(R, C))
        self.FOR1[R][C] = Value
        print(self.FOR1)

    def EGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        Value = int(self.gridElements.GetCellValue(R, C))
        if  Value > self.n_nodes and (C == 1 or C == 2):
            self.ErrorString = 'Node  ' + str(Value) + '  is not defined.'
            app.frame_error = ErrorFrame(self.ErrorString)
            app.frame_error.Show()
        else:
            self.ELE[R][C+1] = Value
                               
    def BCGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        if C == 0:
            Value = int(self.gridBC.GetCellValue(R, C))
            if  Value > self.n_nodes:
                self.ErrorString = 'Node  ' + str(Value) + '  is not defined.'
                app.frame_error = ErrorFrame(self.ErrorString)
                app.frame_error.Show()
            else:
                self.BC[R][C] = Value
        else:
            Value = int(self.gridBC.GetCellValue(R, C))
            self.BC[R][C] = Value
            
    def SGridRight(self,event):
        self.Sec_to_del = event.GetRow()
        SRMenu = wx.Menu()
        item = wx.MenuItem(SRMenu, wx.NewId(), "Delete Section")
        SRMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelSec, item)
        self.PopupMenu(SRMenu)
        
    def MGridRight(self,event):
        self.Mat_to_del = event.GetRow()
        MRMenu = wx.Menu()
        item = wx.MenuItem(MRMenu, wx.NewId(), "Delete Material")
        MRMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelMat, item)
        self.PopupMenu(MRMenu)
        
    def NGridRight(self,event):
        self.Nod_to_del = event.GetRow()
        NRMenu = wx.Menu()
        item = wx.MenuItem(NRMenu, wx.NewId(), "Delete Node")
        NRMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelNod, item)
        self.PopupMenu(NRMenu)

    def EGridRight(self,event):
        self.Ele_to_del = event.GetRow()
        ERMenu = wx.Menu()
        item = wx.MenuItem(ERMenu, wx.NewId(), "Delete Element")
        ERMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelEle, item)
        self.PopupMenu(ERMenu)

    def BCGridRight(self,event):
        self.BC_to_del = event.GetRow()
        BCRMenu = wx.Menu()
        item = wx.MenuItem(BCRMenu, wx.NewId(), "Delete Support")
        BCRMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelBC, item)
        self.PopupMenu(BCRMenu)

    def LGridRight(self,event):
        self.For_to_del = event.GetRow()
        LRMenu = wx.Menu()
        item = wx.MenuItem(LRMenu, wx.NewId(), "Delete Node")
        LRMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelFor, item)
        self.PopupMenu(LRMenu)

    def OnDelSec(self,event):
        self.gridSections.DeleteRows(self.Sec_to_del, 1, updateLabels=True)
        del self.SEC[self.Sec_to_del]
        for i in range(0,len(self.SEC)):
            if self.SEC[i][0] > self.Sec_to_del + 1:
                self.SEC[i][0] = self.SEC[i][0] - 1
                
        for i in range(0,self.n_elements):
            if self.ELE[i][3] > (self.Sec_to_del + 1):
                self.ELE[i][3] = self.ELE[i][3] - 1
                
    def OnDelMat(self,event):
        self.gridMaterials.DeleteRows(self.Mat_to_del, 1, updateLabels=True)
        del self.MAT[self.Mat_to_del]
        for i in range(0,len(self.MAT)):
            if self.MAT[i][0] > self.Mat_to_del + 1:
                self.MAT[i][0] = self.MAT[i][0] - 1
                
        for i in range(0,len(self.SEC)):
            if self.SEC[i][4] > (self.Mat_to_del + 1):
                self.SEC[i][4] = self.SEC[i][4] - 1
                
    def OnDelNod(self,event):
        self.gridNodes.DeleteRows(self.Nod_to_del, 1, updateLabels=True)
        del self.NOD[self.Nod_to_del]
        self.n_nodes = self.n_nodes - 1
        for i in range(0,self.n_nodes):
            if self.NOD[i][0] > self.Nod_to_del + 1:
                self.NOD[i][0] = self.NOD[i][0] - 1

        Dead_el = []  
        for i in range(0,self.n_elements):
            if self.ELE[i][1] == (self.Nod_to_del + 1) or self.ELE[i][2] == (self.Nod_to_del + 1):
                Dead_el.append(i)
            
            if self.ELE[i][1] > (self.Nod_to_del + 1):
                self.ELE[i][1] = self.ELE[i][1] - 1
                self.gridElements.SetCellValue(i,0,str(self.ELE[i][1]))
                
            if self.ELE[i][2] > (self.Nod_to_del + 1):
                self.ELE[i][2] = self.ELE[i][2] - 1
                self.gridElements.SetCellValue(i,1,str(self.ELE[i][2]))

        for i in range(0,len(Dead_el)):
            self.Ele_to_del = Dead_el[i]
            self.OnDelEle(0)
            for j in range(i+1,len(Dead_el)):
                Dead_el[j] = Dead_el[j] - 1

        for i in range(0,self.n_supports):
            if self.BC[i][0] > self.Nod_to_del + 1:
                self.BC[i][0] = self.BC[i][0] - 1
                
        for i in range(0,len(self.FOR1)):
            if self.FOR1[i][0] > self.Nod_to_del + 1:
                self.FOR1[i][0] = self.FOR1[i][0] - 1

    def OnDelEle(self,event):
        self.gridElements.DeleteRows(self.Ele_to_del, 1, updateLabels=True)
        n1 = self.ELE[self.Ele_to_del][1]
        n2 = self.ELE[self.Ele_to_del][2]
        del self.ELE[self.Ele_to_del]
        del self.selected_el[self.Ele_to_del]
        self.n_elements = self.n_elements - 1
        Dead_n1 = False
        Dead_n2 = False
        for i in range(0,self.n_elements):
            if self.ELE[i][0] > self.Ele_to_del + 1:
                self.ELE[i][0] = self.ELE[i][0] - 1
            
            if self.ELE[i][1] == n1 or self.ELE[i][2] == n1:
                Dead_n1 = True
            
            if self.ELE[i][1] == n2 or self.ELE[i][2] == n2:
                Dead_n2 = True
                
        if Dead_n1 == False:
            self.Nod_to_del = n1-1
            self.OnDelNod(0)
            if n2 > n1:
                n2 = n2 - 1
             
        if Dead_n2 == False:
            self.Nod_to_del = n2-1
            self.OnDelNod(0)
            
            
    def OnDelBC(self,event):
        x1 = self.gridBC.GetCellEditor(self.BC_to_del, 1)
        x2 = self.gridBC.GetCellEditor(self.BC_to_del, 2)
        x3 = self.gridBC.GetCellEditor(self.BC_to_del, 3)
        
        x1.Destroy()
        x2.Destroy()
        x3.Destroy() 
        
        self.gridBC.DeleteRows(self.BC_to_del, 1, updateLabels=True)
        
        del self.BC[self.BC_to_del]
        self.n_supports = self.n_supports - 1
#         for i in range(0,self.n_supports):
#             if self.BC[i][0] > self.BC_to_del + 1:
#                 self.BC[i][0] = self.BC[i][0] - 1
                
    def OnDelFor(self,event):
        self.gridForces.DeleteRows(self.For_to_del, 1, updateLabels=True)
        self.FOR1 = np.delete(self.FOR1, self.For_to_del, 0)
        for i in range(0,len(self.FOR1[:,0])):
            if self.FOR1[i][0] > self.For_to_del + 1:
                self.FOR1[i][0] = self.FOR1[i][0] - 1


    def OnClickDoneP(self,event):
        self.nb.ChangeSelection(1)
        self.draw_figure()
        
    def OnClickDoneG(self,event):
        self.nb.ChangeSelection(2)
        self.draw_figure()
        
    def OnClickDoneBC(self,event):
#         self.nb.ChangeSelection(3)
        self.draw_figure()
        
#     def OnClickDoneF(self,event):
#         self.draw_figure()

    def on_labelN(self,event):
        self.draw_figure()
        
    def on_labelE(self,event):
        self.draw_figure()
        
    def on_labelF(self,event):
        self.draw_figure()

    def OnClickViewOptions(self,event):
#         sF = [self.show_F1,self.show_F2,self.show_F3,self.show_F4,self.show_F5,self.show_F6]
#         lF = [self.label_F1,self.label_F2,self.label_F3,self.label_F4,self.label_F5,self.label_F6]
        sF = [self.show_F1]
        lF = [self.label_F1]        
        lG = [self.labelN,self.labelE]
        dlg = ViewOptions(sF,lF,lG)
        dlg.CenterOnScreen()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.show_F1 = dlg.showF1.GetValue()
#             self.show_F2 = dlg.showF2.GetValue()
#             self.show_F3 = dlg.showF3.GetValue()
#             self.show_F4 = dlg.showF4.GetValue()
#             self.show_F5 = dlg.showF5.GetValue()
#             self.show_F6 = dlg.showF6.GetValue()
            
            self.label_F1 = dlg.labelF1.GetValue()
#             self.label_F2 = dlg.labelF2.GetValue()
#             self.label_F3 = dlg.labelF3.GetValue()
#             self.label_F4 = dlg.labelF4.GetValue()
#             self.label_F5 = dlg.labelF5.GetValue()
#             self.label_F6 = dlg.labelF6.GetValue()
            
            self.draw_figure()
        else:
            pass
        dlg.Destroy()

    def OnClickDimensions(self,event):
        dlg = DimDialog(self.Lim_X1,self.Lim_Y1,self.Lim_X2,self.Lim_Y2,self.Grid_Spacing)
        dlg.CenterOnScreen()
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            self.Lim_X1 = dlg.Lim_X1
            self.Lim_Y1 = dlg.Lim_Y1
            self.Lim_X2 = dlg.Lim_X2
            self.Lim_Y2 = dlg.Lim_Y2
            self.Grid_Spacing = dlg.Grid_spacing
            
            self.Dim_X = self.Lim_X2 - self.Lim_X1
            self.Dim_Y = self.Lim_Y2 - self.Lim_Y1
            self.draw_figure()
        else:
            pass
            
        dlg.Destroy()        

        
    def draw_figure(self):
        """ Redraws the figure
        """
        # clear the axes and redraw the plot anew
        #
        col1 = 'black'
        col2 = 'blue'
        col3 = 'blue'
         
        self.axes.clear()
        
        if hasattr(self, 'temp_patch'):
            del self.temp_patch
        
   
        self.axes.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axes.set_ylim([self.Lim_Y1, self.Lim_Y2])
        self.axes.set_xticklabels([])
        self.axes.set_yticklabels([])
        self.x_tick = np.arange(self.Lim_X1,self.Lim_X2,self.Grid_Spacing)
        self.y_tick = np.arange(self.Lim_Y1,self.Lim_Y2,self.Grid_Spacing)
        self.axes.set_xticks(self.x_tick)
        self.axes.set_yticks(self.y_tick)   
        self.axes.grid(self.m_grid.IsChecked())

        for i in range(0,self.n_nodes):
            if self.NOD[i][0] != 0:
                self.axes.scatter(self.NOD[i][1], self.NOD[i][2], color = col1, s=5)
                dist = float(self.Dim_X)/float(60)
                if self.labelN == True:
                    self.axes.text(self.NOD[i][1]+dist,self.NOD[i][2]+dist, 
                                   str(int(self.NOD[i][0])), size='small',color=col1)
         
        for i in range(0,self.n_elements):
            if self.ELE[i][1] != 0 and self.ELE[i][2] != 0:
                X1, X2 = self.NOD[self.ELE[i][1]-1][1], self.NOD[self.ELE[i][2]-1][1]
                Y1, Y2 = self.NOD[self.ELE[i][1]-1][2], self.NOD[self.ELE[i][2]-1][2]
                if self.selected_el[i] == 0:
                    selcol = col1
                elif self.selected_el[i] == 1:
                    selcol = 'red'
                self.axes.plot((X1,X2), (Y1,Y2), color = selcol,linewidth=1.5,picker=0.25*self.Dim_X)
                dist = float(self.Dim_X)/float(60)
                p = ((X2+X1)/2-2*dist,(Y2+Y1)/2+dist)
                if self.labelE == True:
                    self.axes.text(p[0],p[1], 
                                   'E'+str(int(self.ELE[i][0])), size='small',color=col1,alpha=0.5)
             
        for i in range(0,self.n_supports):
            p = [self.NOD[self.BC[i][0]-1][1], self.NOD[self.BC[i][0]-1][2]]
            selcol = col1
            if self.sup_selected[1] == self.BC[i][0]:
                if self.sup_selected[0] == False:
                    selcol = col1
                elif self.sup_selected[0] == True:
                    selcol = 'red'
            if self.BC[i][0] != 0:
#                 if self.BC[i][1] == 1 and self.BC[i][2] == 1 and self.BC[i][3] == 1:
#                     patch = self.get_patch_fixed(p,0.03*self.Dim_X,self.BC[i][4],selcol)
#                     self.axes.add_patch(patch)
                if self.BC[i][1] == 1 and self.BC[i][2] == 1:
                    patch = self.get_patch_hinge(p,0.03*self.Dim_X,self.BC[i][3],selcol)
                    self.axes.add_patch(patch)
                elif self.BC[i][1] == 0 and self.BC[i][2] == 1:
                    patch = self.get_patch_roller(p,0.03*self.Dim_X,self.BC[i][3],selcol)
                    self.axes.add_patch(patch)
                elif self.BC[i][1] == 1 and self.BC[i][2] == 0:
                    patch = self.get_patch_roller(p,0.03*self.Dim_X,90+self.BC[i][3],selcol)
                    self.axes.add_patch(patch)
         

        for i in range(0,len(self.FOR1)):
            if self.FOR1[i][0] != 0:
                p =[self.NOD[int(self.FOR1[i][0])-1][1], self.NOD[int(self.FOR1[i][0])-1][2]]
                selcol = col2
                if self.f1_selected[1] == i:
                    if self.f1_selected[0] == False:
                        selcol = col2
                    elif self.f1_selected[0] == True:
                        selcol = 'red'
                if self.FOR1[i][1] != 0 or self.FOR1[i][2] != 0:
                    scl = 0.005*self.Dim_X
                    f1 = scl*self.FOR1[i][1]
                    f2 = scl*self.FOR1[i][2]
                    if f1 > 0 and f2 >= 0:
                        ang = np.arctan(f2/f1) - np.pi/2
                    elif f1 < 0 and f2 >= 0:
                        ang = np.arctan(f2/f1) + np.pi/2
                    elif f1 < 0 and f2 <= 0:
                        ang = np.arctan(f2/f1) + np.pi/2
                    elif f1 > 0 and f2 <= 0:
                        ang = np.arctan(f2/f1) - np.pi/2
                    elif f1 == 0 and f2 >= 0: 
                        ang = 0
                    elif f1 == 0 and f2 <= 0: 
                        ang = np.pi
                        
                    patchF1 = self.get_patch_arrow(p, 0.03*self.Dim_X, ang, (p[0]-f1,p[1]-f2), selcol)
                    self.axes.add_patch(patchF1)
                    
                    if self.label_F1 == True:
                        self.axes.text(p[0]-f1+self.Dim_X/100,p[1]-f2+self.Dim_X/100,
                                        str(int((self.FOR1[i][1]**2+self.FOR1[i][2]**2)**0.5)), size='small',color=selcol,alpha=0.7 )
                else:
                    ang = 0
                    
#                     if self.FOR1[i][3] != 0:
#                         if self.FOR1[i][1] == 0 and self.FOR1[i][2] == 0:
#                             pick = True
#                         else:
#                             pick = False
#                             
#                         patchM1 = self.get_patch_moment(p, 0.02*self.Dim_X, ang+np.pi, (0,0), selcol, pick)
#                         self.axes.add_patch(patchM1)
#                         
#                         if self.label_F1 == True:
#                             self.axes.text(p[0]-np.sin(ang+np.pi)*self.Dim_X/50,p[1]+np.cos(ang+np.pi)*self.Dim_X/50,
#                                             str(int(self.FOR1[i][3])), size='small',color=selcol,alpha=0.7 )

#         if self.show_F2 == True and len(self.FOR2) != 0:
#             for i in range(0,len(self.FOR2)):
#                 if self.FOR2[i][0] != 0:
#                     [x1,y1,x2,y2,phi] = self.get_element_properties(self.FOR2[i][0]-1,['x1','y1','x2','y2','phi'])
#                     phi_inv = -1*phi
#                     p =[x1+(x2-x1)*self.FOR2[i][4], y1+(y2-y1)*self.FOR2[i][4]]
#                     selcol = col2
#                     if self.f2_selected[1] == i:
#                         if self.f2_selected[0] == False:
#                             selcol = col2
#                         elif self.f2_selected[0] == True:
#                             selcol = 'red'
#                     if self.FOR2[i][1] != 0 or self.FOR2[i][2] != 0:
#                         if self.f2_CS[i] == 1:
#                             F21 = np.cos(phi_inv)*self.FOR2[i][1] - np.sin(phi_inv)*self.FOR2[i][2]
#                             F22 = np.sin(phi_inv)*self.FOR2[i][1] + np.cos(phi_inv)*self.FOR2[i][2]
#                         elif self.f2_CS[i] == 0:
#                             F21 = np.cos(phi)*self.FOR2[i][1] - np.sin(phi)*self.FOR2[i][2]
#                             F22 = np.sin(phi)*self.FOR2[i][1] + np.cos(phi)*self.FOR2[i][2]
# 
#                         scl = 0.005*self.Dim_X
#                         
#                         f1 = scl*F21
#                         f2 = scl*F22                        
#                         if f1 > 0 and f2 >= 0:
#                             ang = np.arctan(f2/f1) - np.pi/2
#                         elif f1 < 0 and f2 >= 0:
#                             ang = np.arctan(f2/f1) + np.pi/2
#                         elif f1 < 0 and f2 <= 0:
#                             ang = np.arctan(f2/f1) + np.pi/2
#                         elif f1 > 0 and f2 <= 0:
#                             ang = np.arctan(f2/f1) - np.pi/2
#                         elif f1 == 0 and f2 >= 0: 
#                             ang = 0
#                         elif f1 == 0 and f2 <= 0: 
#                             ang = np.pi
# 
#                         patchF2 = self.get_patch_arrow(p, 0.03*self.Dim_X, ang, (p[0]-f1,p[1]-f2), selcol)
#                         self.axes.add_patch(patchF2)
#                         
#                         if self.label_F2 == True:
#                             self.axes.text(p[0]-f1+self.Dim_X/100,p[1]-f2+self.Dim_X/100,
#                                            str(int((self.FOR2[i][1]**2+self.FOR2[i][2]**2)**0.5)), size='small',color=selcol,alpha=0.7 )
# 
#                     else:
#                         ang = 0
# 
#                         
#                     if self.FOR2[i][3] != 0:
#                         if self.FOR2[i][1] == 0 and self.FOR2[i][2] == 0:
#                             pick = True
#                         else:
#                             pick = False
#                             
#                         patchM2 = self.get_patch_moment(p, 0.02*self.Dim_X, ang+np.pi, (0,0), selcol, pick)
#                         self.axes.add_patch(patchM2)
#                         
#                         if self.label_F2 == True:
#                             self.axes.text(p[0]-np.sin(ang+np.pi)*self.Dim_X/50,p[1]+np.cos(ang+np.pi)*self.Dim_X/50,
#                                             str(int(self.FOR2[i][3])), size='small',color=selcol,alpha=0.7 )
#                  
#         if self.show_F3 == True and len(self.FOR3) != 0:
#             for i in range(0,len(self.FOR3)):
#                 if self.FOR3[i][0] != 0:
#                     [x1,y1,x2,y2,phi] = self.get_element_properties(self.FOR3[i][0]-1,['x1','y1','x2','y2','phi'])
# 
#                     phi_inv = -1*phi
#                     selcol = col2
#                     if self.f3_selected[1] == i:
#                         if self.f3_selected[0] == False:
#                             selcol = col2
#                         elif self.f3_selected[0] == True:
#                             selcol = 'red'
#                     if self.FOR3[i][1] != 0 or self.FOR3[i][2] != 0:
#                         if self.f3_CS[i] == 1:
#                             if self.FOR3[i][2] > 0:
#                                 if y1 < y2:
#                                     y2 = y1
#                                 else:
#                                     y1 = y2
#                             else:
#                                 if y1 > y2:
#                                     y2 = y1
#                                 else:
#                                     y1 = y2
#                             F31 = np.cos(phi_inv)*self.FOR3[i][1] - np.sin(phi_inv)*self.FOR3[i][2]
#                             F32 = np.sin(phi_inv)*self.FOR3[i][1] + np.cos(phi_inv)*self.FOR3[i][2]
#                         elif self.f3_CS[i] == 0:
#                             F31 = np.cos(phi)*self.FOR3[i][1] - np.sin(phi)*self.FOR3[i][2]
#                             F32 = np.sin(phi)*self.FOR3[i][1] + np.cos(phi)*self.FOR3[i][2]
# 
#                         scl = 0.005*self.Dim_X
#                         
#                         f1 = scl*F31
#                         f2 = scl*F32                        
#                         if f1 > 0 and f2 >= 0:
#                             ang = np.arctan(f2/f1) - np.pi/2
#                         elif f1 < 0 and f2 >= 0:
#                             ang = np.arctan(f2/f1) + np.pi/2
#                         elif f1 < 0 and f2 <= 0:
#                             ang = np.arctan(f2/f1) + np.pi/2
#                         elif f1 > 0 and f2 <= 0:
#                             ang = np.arctan(f2/f1) - np.pi/2
#                         elif f1 == 0 and f2 >= 0: 
#                             ang = 0
#                         elif f1 == 0 and f2 <= 0: 
#                             ang = np.pi
#                         
#                         val = (self.FOR3[i][1]**2 + self.FOR3[i][2]**2)**0.5
#                         patchF3 = self.get_patch_distributed(x1,y1,x2,y2,ang,val,0.02*self.Dim_X,selcol)
#                         self.axes.add_patch(patchF3)
#                         
#                         if self.label_F3 == True:
#                             self.axes.text((x1+x2)/2-self.FOR3[i][1]*scl+self.Dim_X/50,(y1+y2)/2-self.FOR3[i][2]*scl+self.Dim_X/50,
#                                            str(int(val)), size='small',color=selcol,alpha=0.7 )
#          
#         if self.show_F4 == True and len(self.FOR4) != 0:
#             for i in range(0,len(self.FOR4)):
#                 if self.FOR4[i][0] != 0:
#                     [x1,y1,x2,y2,grad] = self.get_element_properties(self.FOR4[i][0]-1,['x1','y1','x2','y2','phi'])
#                      
#                     rect1,rect2 = self.get_patch_temperature(x1,y1,x2,y2,grad)
#                     self.axes.add_patch(rect1)
#                     self.axes.add_patch(rect2)
#  
#         if self.show_F5 == True and len(self.FOR5) != 0:
#             for i in range(0,len(self.FOR5)):
#                 if self.FOR5[i][0] != 0:
#                     x1, y1 = self.NOD[int(self.FOR5[i][0])-1][1], self.NOD[int(self.FOR5[i][0])-1][2]
#                     if self.FOR5[i][1] != 0:
#                         sign = self.FOR5[i][1]/abs(self.FOR5[i][1])
#                         x2, y2 = x1 + sign*self.Dim_X/12, y1
#                         x3, y3 = x2, y2 + self.Dim_X/30
#                         x4, y4 = x2, y2 - self.Dim_X/30
#                         self.axes.plot([x1,x2],[y1,y2],'black',linestyle='--')
#                         self.axes.plot([x3,x4],[y3,y4],'black')
#                         if self.label_F5 == True:
#                             self.axes.text(x2 + sign*self.Dim_X/30,y2,
#                                             str(sign*self.FOR5[i][1]), size='x-small',color='black',alpha=0.7 ) 
#                      
#                     if self.FOR5[i][2] != 0:
#                         sign = self.FOR5[i][2]/abs(self.FOR5[i][2])
#                         x2, y2 = x1, y1 + sign*self.Dim_X/12
#                         x3, y3 = x2 + self.Dim_X/30, y2
#                         x4, y4 = x2 - self.Dim_X/30, y2
#                         self.axes.plot([x1,x2],[y1,y2],'black',linestyle='--')
#                         self.axes.plot([x3,x4],[y3,y4],'black')
#                         if self.label_F5 == True:
#                             self.axes.text(x2,y2 + sign*self.Dim_X/30,
#                                             str(sign*self.FOR5[i][2]), size='x-small',color='black',alpha=0.7 )
#                              
#                     if self.FOR5[i][3] != 0:
#                         sign = self.FOR5[i][3]/abs(self.FOR5[i][3])
#                         x2, y2 = x1 + sign*self.Dim_X/20, y1 + self.Dim_X/40
#                         x3, y3 = x1 - sign*self.Dim_X/20, y1 - self.Dim_X/40
#                         self.axes.plot([x2,x3],[y2,y3],'black')
#                         if self.label_F5 == True:
#                             self.axes.text(x1+self.Dim_X/20,y1+sign*self.Dim_X/30,
#                                             str(sign*self.FOR5[i][3]), size='x-small',color='black',alpha=0.7 )  
#  
#                  
        self.canvas.draw()

    def ClearGrids(self):
        R = self.gridSections.GetNumberRows()
        if R > 0:
            self.gridSections.DeleteRows(pos=0, numRows=R, updateLabels=True)

        R = self.gridMaterials.GetNumberRows()
        if R > 0:
            self.gridMaterials.DeleteRows(pos=0, numRows=R, updateLabels=True)

        R = self.gridNodes.GetNumberRows()
        if R > 0:
            self.gridNodes.DeleteRows(pos=0, numRows=R, updateLabels=True)

        R = self.gridElements.GetNumberRows()
        if R > 0:
            self.gridElements.DeleteRows(pos=0, numRows=R, updateLabels=True)
            
        for i in range(0,self.n_supports):
            x1 = self.gridBC.GetCellEditor(self.n_supports-i-1, 1)
            x2 = self.gridBC.GetCellEditor(self.n_supports-i-1, 2)
            x3 = self.gridBC.GetCellEditor(self.n_supports-i-1, 3)
            
            x1.Destroy()
            x2.Destroy()
            x3.Destroy()             
            
            self.gridBC.DeleteRows(self.n_supports-i-1, 1, updateLabels=True)


    def Initialization(self):
        self.n_nodes = 0
        self.n_elements = 0
        self.n_supports = 0
        self.n_loads = 0

        self.VARS = []
        self.OBJ = 1
        self.CONSTR = np.zeros((4,3))
        self.OPT_PAR = [40, 1000, 0.8, 0.5, 0.8, 6, 20, 7, 0, 1, 0, 1000]
        
        self.PAR = [1, 1, 0, 1e-12, 15, 1e-6, 16, 1e-9, 1000, 1e-9, 1000, 1000]
        self.MAT = [[1, 7850, 21000000]]
        self.SEC = [[1, 0.12, 1]]

        self.NOD = []
        self.ELE = []
        self.BC = []
        
        self.FOR1 = []
        
        self.f2_CS = []
        self.f3_CS = []
        
        self.selected_el = []
        self.sup_selected = [False,0]
        
        self.f1_selected = [False,0]
        self.f2_selected = [False,0]
        self.f3_selected = [False,0]
        self.f4_selected = [False,0]
        self.f5_selected = [False,0]
        self.f6_selected = [False,0]

        self.case = 0        

    def on_new(self, event):
        self.on_open(0)

    def on_open(self, event):
        if event != 0:
            dlg = wx.FileDialog(self, message="Choose a file",defaultDir=os.getcwd(), \
                                defaultFile="",style=wx.FD_OPEN | wx.FD_MULTIPLE | wx.FD_CHANGE_DIR)
    
            if dlg.ShowModal() == wx.ID_OK:
                path = dlg.GetPaths()
                
                self.ClearGrids()
                self.Initialization()
            
            
            dlg.Destroy()
            defaultPath = path[0]
        else:
            Dir=os.getcwd()
            # Windows iOS
            defaultPath = Dir + '/New.txt' 
            
        f = open(defaultPath)
        lines = f.readlines()
        f.close()
        for i in range(0,len(lines)):
            k = list(map(str, lines[i].split()))
            if k[0] == '*NODES':
                nod = i
            elif k[0] == '*ELEMENTS':
                ele = i
            elif k[0] == '*MATERIAL':
                mat = i
            elif k[0] == '*SECTION':
                sec = i
            elif k[0] == '*BC':
                bc = i
            elif k[0] == '*FOR':
                forn = i
            elif k[0] == '*OPT_VARIABLES':
                var = i
            elif k[0] == '*OBJECTIVE':
                obj = i
            elif k[0] == '*CONSTRAINTS':
                constr = i
            elif k[0] == '*PARAMETERS':
                par = i
            elif k[0] == '*OPT_PARAMETERS':
                par_opt = i
    
        nodes = []
        for i in range(nod+1,ele):
            nodes.append(list(map(float, lines[i].split())))
        
        elements = []
        for i in range(ele+1,mat):
            elements.append(list(map(int, lines[i].split())))

        materials = []
        for i in range(mat+1,sec):
            materials.append(list(map(float, lines[i].split())))
        
        sections = []
        for i in range(sec+1,bc):
            sections.append(list(map(float, lines[i].split())))
        
        BCs = []
        for i in range(bc+1,forn):
            BCs.append(list(map(int, lines[i].split())))
        
        forces = []
        for i in range(forn+1,var):
            forces.append(list(map(float, lines[i].split())))
            
        variables = []
        for i in range(var+1,obj):
            variables.append(list(map(float, lines[i].split())))
            
        objective = list(map(float, lines[obj+1].split()))
            
        constraints = []
        for i in range(constr+1,par):
            constraints.append(list(map(float, lines[i].split())))


        parameters = list(map(float, lines[par+1].split()))
        parameters_opt = list(map(float, lines[par_opt+1].split()))

        self.NOD  = nodes
        self.ELE  = elements
        self.MAT  = materials
        self.SEC  = sections
        self.BC   = BCs
        self.FOR1 = forces
        
        self.VARS = variables
        self.OBJ  = objective[0]
        self.CONSTR = constraints
        self.PAR  = parameters
        self.OPT_PAR = parameters_opt

#         print self.OPT_PAR

        self.n_nodes    = len(self.NOD)
        self.n_elements = len(self.ELE)
        self.n_supports = len(self.BC)
        
#         self.selected_el = []
        for i in range(0,self.n_elements):
            self.selected_el.append(0)

        self.gridSections.AppendRows(len(self.SEC), updateLabels = True)
        for i in range(0,len(self.SEC)):
            for j in range(0,2):
                if j == 4:
                    self.gridSections.SetCellValue(i,j,str(int(self.SEC[i][j+1])))
                else:
                    self.gridSections.SetCellValue(i,j,str(self.SEC[i][j+1]))
                    
        
        self.gridMaterials.AppendRows(len(self.MAT), updateLabels = True)
        for i in range(0,len(self.MAT)):
            for j in range(0,2):
                self.gridMaterials.SetCellValue(i,j,str(self.MAT[i][j+1]))
        
        if event != 0: 
            self.gridNodes.AppendRows(len(self.NOD), updateLabels = True)
            max_X = 0
            min_X = 10**10
            max_Y = 0
            min_Y = 10**10
            for i in range(0,len(self.NOD)):
                self.gridNodes.SetCellValue(i,0,str(self.NOD[i][1]))
                self.gridNodes.SetCellValue(i,1,str(self.NOD[i][2]))
                
                if self.NOD[i][1] > max_X:
                    max_X = self.NOD[i][1]
                    
                if self.NOD[i][1] < min_X:
                    min_X = self.NOD[i][1] 
                    
                if self.NOD[i][2] > max_Y:
                    max_Y = self.NOD[i][2]
                    
                if self.NOD[i][2] < min_Y:
                    min_Y = self.NOD[i][2]               
            
            self.Lim_X1 = min_X - 3
            self.Lim_Y1 = min_Y - 3
            self.Lim_X2 = max_X + 3
            self.Lim_Y2 = max_Y + 3
            
        self.gridElements.AppendRows(len(self.ELE), updateLabels = True)
        for i in range(0,len(self.ELE)):
            self.gridElements.SetCellValue(i,0,str(self.ELE[i][1]))
            self.gridElements.SetCellValue(i,1,str(self.ELE[i][2]))
            self.gridElements.SetCellValue(i,2,str(self.ELE[i][3]))

        for i in range(0,len(self.BC)):
            self.gridBC.AppendRows(1, updateLabels = True)
            self.gridBC.SetCellEditor(i+1, 1, self.choice_editor)
            self.gridBC.SetCellEditor(i+1, 2, self.choice_editor)
            self.gridBC.SetCellEditor(i+1, 3, self.choice_editor)
        for i in range(0,len(self.BC)):
            for j in range(0,4):
                self.gridBC.SetCellValue(i,j,str(self.BC[i][j]))

        self.gridForces.AppendRows(len(self.FOR1), updateLabels = True)
        for i in range(0,len(self.FOR1)):
            self.gridForces.SetCellValue(i,0,str(self.FOR1[i][0]))
            self.gridForces.SetCellValue(i,1,str(self.FOR1[i][1]))
            self.gridForces.SetCellValue(i,2,str(self.FOR1[i][2]))
             
        self.draw_figure()
        
    def on_save(self, event):
        wildcard = "Text file (*.txt)|*.txt|"        \
           "All files (*.*)|*.*"

        dlg = wx.FileDialog(
            self, message="Save file as ...", defaultDir=os.getcwd(), 
            defaultFile="",wildcard=wildcard, style=wx.FD_SAVE
            )

        # This sets the default filter that the user will initially see. Otherwise,
        # the first filter in the list will be used by default.
        dlg.SetFilterIndex(0)

        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            self.Create_Input_File(path)

        dlg.Destroy()
        
    def on_exit(self, event):
        self.Destroy()
        
    def on_about(self, event):
        msg = """ A demo using wxPython with matplotlib:
        
         * Use the matplotlib navigation bar
         * Add values to the text box and press Enter (or click "Draw!")
         * Show or hide the grid
         * Drag the slider to modify the width of the bars
         * Save the plot to a file using the File menu
         * Click on a bar to receive an informative message
        """
        dlg = wx.MessageDialog(self, msg, "About", wx.OK)
        dlg.ShowModal()
        dlg.Destroy()

    def OnClickInput(self):
        #self.Create_Input_File('input.txt')
        self.input_OK = True
        cond = True
#         for i in range(0,len(self.SEC[:,0])):
#             if self.SEC[i][1] == 0:
#                 self.WarningString = 'No density assigned to Section  ' + str(i+1) + '.'
#                 app.frame_Warning = WarningFrame(self.WarningString)
#                 app.frame_Warning.Show()
#             elif self.SEC[i][2] == 0:
#                 self.WarningString = 'No young modulus E assigned to Section  ' + str(i+1) + '.'
#                 app.frame_Warning = WarningFrame(self.WarningString)
#                 app.frame_Warning.Show()
#             elif self.SEC[i][3] == 0:
#                 self.WarningString = 'No Area assigned to Section  ' + str(i+1) + '.'
#                 app.frame_Warning = WarningFrame(self.WarningString)
#                 app.frame_Warning.Show()
 
        if cond == True:
            cond = False    
            for j in range(0,len(self.ELE)):
                if (self.ELE[j][1] == 0):
                    self.ErrorString = 'Node N1 of element  ' + str(j+1) + '  is not defined.'
                    app.frame_error = ErrorFrame(self.ErrorString)
                    app.frame_error.Show()
                    self.input_OK = False    
                elif self.ELE[j][2]  == 0:
                    self.ErrorString = 'Node N2 of element  ' + str(j+1) + '  is not defined.'
                    app.frame_error = ErrorFrame(self.ErrorString)
                    app.frame_error.Show()
                    self.input_OK = False
                elif self.ELE[j][3]  == 0:
                    self.ErrorString = 'Element  ' + str(j+1) + '  has no section.'
                    app.frame_error = ErrorFrame(self.ErrorString)
                    app.frame_error.Show()
                    self.input_OK = False                        
                else:
                    cond = True
                 
            if cond == True:
                cond = False    
                for k in range(0,len(self.BC)):
                    if self.BC[k][0] == 0:
                        self.ErrorString = 'No node assigned to support  ' + str(k+1) + ' .'
                        app.frame_error = ErrorFrame(self.ErrorString)
                        app.frame_error.Show()
                        self.input_OK = False
                    else:
                        cond = True
                        
        if self.input_OK == True and cond == True:
            Path = self.PATH + '\input.txt'
            self.Create_Input_File(Path)


    def Create_Input_File(self,filename):   
        f = open(filename,'w')
        f.write('*NODES\n')
        for i in range(0,len(self.NOD)):
            f.write(str(int(self.NOD[i][0])) + '  ' )
            for j in range(1,3):
                f.write(str(float(self.NOD[i][j])) + '  ' )
            f.write('\n')
            
        f.write('*ELEMENTS\n')
        for i in range(0,len(self.ELE)):
            for j in range(0,4):
                f.write(str(int(self.ELE[i][j])) + '  ')
            f.write('\n')

        f.write('*MATERIAL\n')
        for i in range(0,len(self.MAT)):
            f.write(str(int(self.MAT[i][0])) + '  ' )
            for j in range(1,3):
                f.write(str(float(self.MAT[i][j])) + '  ') 
            f.write('\n')
            
        f.write('*SECTION\n')
        for i in range(0,len(self.SEC)):
            f.write(str(int(self.SEC[i][0])) + '  ' )
            for j in range(1,3):
                f.write(str(float(self.SEC[i][j])) + '  ') 
            f.write('\n')
            
        f.write('*BC\n')
        for i in range(0,len(self.BC)):
            for j in range(0,4):
                f.write(str(int(self.BC[i][j])) + '  ')
            f.write('\n')

        f.write('*FOR\n')
        if len(self.FOR1) != 0:
            if (self.FOR1[0][0] == 0 and len(self.FOR1) == 1) or len(self.FOR1) == 0:
                pass
            else:
                for i in range(0,len(self.FOR1)):
                    f.write(str(int(self.FOR1[i][0])) + '  ' )
                    for j in range(1,3):
                        f.write(str(float(self.FOR1[i][j])) + '  ')    
                    f.write('\n')
#         print self.VARS            
        f.write('*OPT_VARIABLES\n')
        for i in range(0,len(self.VARS)):
            f.write(str(int(self.VARS[i][0])) + '  ')
            f.write(str(int(self.VARS[i][1])) + '  ')
            f.write(str(int(self.VARS[i][2])) + '  ')
            f.write(str(float(self.VARS[i][3])) + '  ')
            f.write(str(float(self.VARS[i][4])) + '  ')
            f.write('\n')
        
        f.write('*OBJECTIVE\n')
        f.write(str(int(self.OBJ)) + '  ')
        f.write('\n')

        f.write('*CONSTRAINTS\n')
        for i in range(0,len(self.CONSTR)):
            f.write(str(int(self.CONSTR[i][0])) + '  ')
            f.write(str(float(self.CONSTR[i][1])) + '  ')
            f.write(str(float(self.CONSTR[i][2])) + '  ')
            f.write('\n')

        f.write('*PARAMETERS\n')
        if len(self.PAR) != 0:
            if (self.PAR[0] == 0):
                pass
            else:
                f.write(str(int(  self.PAR[0])) + '  ')
                f.write(str(int(  self.PAR[1])) + '  ')
                f.write(str(int(  self.PAR[2])) + '  ')
                f.write(str(float(self.PAR[3])) + '  ')
                f.write(str(int(  self.PAR[4])) + '  ')
                f.write(str(float(self.PAR[5])) + '  ')
                f.write(str(int(  self.PAR[6])) + '  ')
                f.write(str(float(self.PAR[7])) + '  ')
                f.write(str(int(  self.PAR[8])) + '  ')
                f.write(str(float(self.PAR[9])) + '  ')
                f.write(str(int(  self.PAR[10])) + '  ')
                f.write(str(int(  self.PAR[11])) + '  ')
                f.write('\n')

        f.write('*OPT_PARAMETERS\n')
        if len(self.OPT_PAR) != 0:
#             print self.OPT_PAR
            if (self.OPT_PAR[0] == 0):
                pass
            else:
                f.write(str(int(  self.OPT_PAR[0])) + '  ')
                f.write(str(int(  self.OPT_PAR[1])) + '  ')
                f.write(str(float(self.OPT_PAR[2])) + '  ')
                f.write(str(float(self.OPT_PAR[3])) + '  ')
                f.write(str(float(self.OPT_PAR[4])) + '  ')
                f.write(str(int(  self.OPT_PAR[5])) + '  ')
                f.write(str(int(  self.OPT_PAR[6])) + '  ')
                f.write(str(int(  self.OPT_PAR[7])) + '  ')
                f.write(str(int(  self.OPT_PAR[8])) + '  ')
                f.write(str(int(  self.OPT_PAR[9])) + '  ')
                f.write(str(int(  self.OPT_PAR[10])) + '  ')
                f.write(str(int(  self.OPT_PAR[11])) + '  ')
                f.write('\n')
                
        f.write('*END\n')
        f.close()

    def OnClickCalc(self):
        os.chdir(self.PATH) #Set CWD for the fortran

#        Path = self.PATH + r'\OPT_Truss_DE.exe'
        Path = self.PATH + r'/bin/OpTrusDE'
        
        subprocess.call(Path, shell=False)
 
        app.frame_out = OutputFrame(self.Lim_X1, self.Lim_Y1, self.Lim_X2, self.Lim_Y2, 
                                    self.NOD, self.ELE, self.BC, self.SEC, self.FOR1, self.FOR2, self.FOR3,
                                    self.accuracy, self.PATH)
        
        app.frame_out.Show()

class ErrorFrame(wx.Frame):
    title = 'Error!'
    def __init__(self, ErrorString):
        wx.Frame.__init__(self, None, -1, self.title, size=(350,150))
        
        vboxError = wx.BoxSizer(wx.VERTICAL)
        
        self.quoteError = wx.StaticText(self, label=ErrorString)
        vboxError.Add(self.quoteError, 0, border=20, flag=wx.TOP | wx.ALIGN_CENTER)
        
        self.buttonError = wx.Button(self, label="OK")
        vboxError.Add(self.buttonError, 0, border=20, flag=wx.TOP | wx.ALIGN_CENTER)
        self.Bind(wx.EVT_BUTTON, self.OnClickError, self.buttonError)
        
        self.SetSizer(vboxError)
        
    def OnClickError(self, event):
        app.frame_error.Destroy()

class WarningFrame(wx.Frame):
    title = 'Warning!'
    def __init__(self, WarningString):
        wx.Frame.__init__(self, None, -1, self.title, size=(350,150))
        
        vboxWarning = wx.BoxSizer(wx.VERTICAL)
        
        self.quoteWarning = wx.StaticText(self, label=WarningString)
        vboxWarning.Add(self.quoteWarning, 0, border=20, flag=wx.TOP | wx.ALIGN_CENTER)
        
        self.buttonWarning = wx.Button(self, label="OK")
        vboxWarning.Add(self.buttonWarning, 0, border=20, flag=wx.TOP | wx.ALIGN_CENTER)
        self.Bind(wx.EVT_BUTTON, self.OnClickWarning, self.buttonWarning)
        
        self.SetSizer(vboxWarning)
        
    def OnClickWarning(self, event):
        app.frame_Warning.Destroy()        


if __name__ == '__main__':
    
    def alignToBottomRight(win):
        dw, dh = wx.DisplaySize()
        w, h = win.GetSize()
        x = dw - w
        y = dh - h
        win.SetPosition((x/2, y/2))
    
    app = wx.App(False)
    app.frame = MainFrame()
    app.frame.Show()
    size = app.frame.GetSize()
    app.frame.SetSize((780,750))
    app.frame.SetMinSize((size[0] / 2, size[1] / 2))
    alignToBottomRight(app.frame)
    app.frame.Layout()
    app.MainLoop()
