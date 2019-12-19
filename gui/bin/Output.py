import wx
# import wx.grid
# import wx.lib.scrolledpanel as scrolled
import os
import numpy as np

from copy import deepcopy

from matplotlib.figure import Figure
from matplotlib.backends.backend_wxagg import \
    FigureCanvasWxAgg as FigCanvas, \
    NavigationToolbar2WxAgg as NavigationToolbar

from matplotlib.path import Path
import matplotlib.patches as patches

class OutputFrame(wx.Dialog):
    title = 'StructuralForms - Output'
    def __init__(self, Lim_X1, Lim_Y1, Lim_X2, Lim_Y2, NOD, ELE, BC, SEC, FOR1, PATH):
        self.Lim_X1 = Lim_X1
        self.Lim_Y1 = Lim_Y1
        self.Lim_X2 = Lim_X2
        self.Lim_Y2 = Lim_Y2
        self.accuracy = 1000
        
        self.Dim_X = self.Lim_X2 - self.Lim_X1
        self.Dim_Y = self.Lim_Y2 - self.Lim_Y1

        self.NOD  = np.asarray(NOD)
        self.ELE  = np.asarray(ELE)
        self.BC   = np.asarray(BC)
        self.SEC  = np.asarray(SEC)
        self.FOR1 = np.asarray(FOR1)
        
        self.PATH = PATH
        
        self.ACCEPT = False
        
        wx.Frame.__init__(self, None, -1, self.title)
        
        Pth = self.PATH + '/initial_out.txt'
        f = open(Pth)
        lines = f.readlines()
        f.close()
        for i in range(0,len(lines)):
            k = list(map(str, lines[i].split()))
            if (k[0] == 'NODAL') and (k[1] == 'FORCES'):
                P_nod = i+2
            elif (k[0] == 'NODAL') and (k[1] == 'DISPLACEMENTS'):
                D_nod = i+2
            elif (k[0] == 'ELEMENT') and (k[1] == 'FORCES'):
                P_el  = i+2
            elif (k[0] == 'TOTAL') and (k[1] == 'MASS'):
                M_t  = i+1
            elif (k[0] == 'EXTERNAL') and (k[1] == 'WORK'):
                W_e  = i+1
            elif (k[0] == 'MAXIMUM') and (k[1] == 'STRESS'):
                S_m  = i+1
            elif (k[0] == 'EIGENVALUES'):
                Egv  = i+1
            elif (k[0] == 'EIGENVECTORS'):
                EgV  = i+1
                     
        self.P_nodal = []
        for i in range(P_nod,D_nod-2):
            self.P_nodal.append(list(map(float, lines[i].split())))
        
        self.D_nodal = []
        for i in range(D_nod,P_el-2):
            self.D_nodal.append(list(map(float, lines[i].split())))
        
        self.P_element = []
        for i in range(P_el,M_t-1):
            self.P_element.append(list(map(float, lines[i].split())))

        self.M_tot = []
        for i in range(M_t,W_e-1):
            self.M_tot.append(list(map(float, lines[i].split())))
            
        self.W_ext = []
        for i in range(W_e,S_m-1):
            self.W_ext.append(list(map(float, lines[i].split())))

        self.S_max = []
        for i in range(S_m,Egv-1):
            self.S_max.append(list(map(float, lines[i].split())))

        self.E_val = []
        for i in range(Egv,EgV-1):
            self.E_val.append(list(map(float, lines[i].split())))
            
        self.E_vect = []
        for i in range(EgV,len(lines)):
            self.E_vect.append(list(map(float, lines[i].split())))

        
        self.P_nodal = np.asarray(self.P_nodal)
        self.D_nodal = np.asarray(self.D_nodal)
        self.P_element = np.asarray(self.P_element)
        
        self.E_val = np.asarray(self.E_val)
        self.E_vect =np.asarray(self.E_vect)

        Pth = self.PATH + '/optimum_out.txt'
        f = open(Pth)
        lines = f.readlines()
        f.close()

        self.P_nodal_opt = []
        for i in range(P_nod,D_nod-2):
            self.P_nodal_opt.append(list(map(float, lines[i].split())))
        
        self.D_nodal_opt = []
        for i in range(D_nod,P_el-2):
            self.D_nodal_opt.append(list(map(float, lines[i].split())))
        
        self.P_element_opt = []
        for i in range(P_el,M_t-1):
            self.P_element_opt.append(list(map(float, lines[i].split())))

        self.M_tot_opt = []
        for i in range(M_t,W_e-1):
            self.M_tot_opt.append(list(map(float, lines[i].split())))
            
        self.W_ext_opt = []
        for i in range(W_e,S_m-1):
            self.W_ext_opt.append(list(map(float, lines[i].split())))

        self.S_max_opt = []
        for i in range(S_m,Egv-1):
            self.S_max_opt.append(list(map(float, lines[i].split())))

        self.E_val_opt = []
        for i in range(Egv,EgV-1):
            self.E_val_opt.append(list(map(float, lines[i].split())))
            
        self.E_vect_opt = []
        for i in range(EgV,len(lines)):
            self.E_vect_opt.append(list(map(float, lines[i].split())))

        self.P_nodal_opt = np.asarray(self.P_nodal_opt)
        self.D_nodal_opt = np.asarray(self.D_nodal_opt)
        self.P_element_opt = np.asarray(self.P_element_opt)
        
        self.E_val_opt = np.asarray(self.E_val_opt)
        self.E_vect_opt =np.asarray(self.E_vect_opt)

        Pth = self.PATH + '/optimum_design.txt'
        f = open(Pth)
        lines = f.readlines()
        f.close()
        
        for i in range(0,len(lines)):
            k = list(map(str, lines[i].split()))
            if (k[0] == 'NODES'):
                nod_opt = i+1
            elif (k[0] == 'ELEMENT'):
                el_opt = i+1
                
        self.NOD_opt = []
        for i in range(nod_opt,el_opt-1):
            self.NOD_opt.append(list(map(float, lines[i].split())))
            
        self.ELE_opt = []
        for i in range(el_opt,len(lines)):
            self.ELE_opt.append(list(map(float, lines[i].split())))
            
        self.NOD_opt = np.asarray(self.NOD_opt)
        self.ELE_opt = np.asarray(self.ELE_opt)

        for i in range(0,len(self.NOD_opt[:,1])):
            self.NOD_opt[i,1] = round(1000*self.NOD_opt[i,1])/1000
            self.NOD_opt[i,2] = round(1000*self.NOD_opt[i,2])/1000
        
        for i in range(0,len(self.ELE_opt[:,1])):
            self.ELE_opt[i,1] = round(1000*self.ELE_opt[i,1])/1000
        
        print(type(self.D_nodal[:,2]))
        print(np.abs(self.D_nodal[:,1]))
        
        maxval = np.max([np.max(np.abs(self.D_nodal[:,1])),np.max(np.abs(self.D_nodal[:,2]))])
        print('max',maxval)
        self.Scale_factor = int(self.Dim_X/20/maxval)

#         os.remove('initial_out.txt')
#         os.remove('optimum_out.txt')
#         os.remove('optimum_design.txt')
        
        self.create__panel()

    def create__panel(self):

        self.dpi = 100

        self.panel = wx.Panel(self)
        
        self.nb = wx.Notebook(self.panel)
        
        self.GeometryTab = wx.Window(self.nb)
        self.ForceTab = wx.Window(self.nb)
        self.DisplacementTab = wx.Window(self.nb)
        self.ModeTab = wx.Window(self.nb)
        
        self.nb.AddPage(self.GeometryTab, "Geometry")
        self.nb.AddPage(self.ForceTab, "Forces")
        self.nb.AddPage(self.DisplacementTab, "Displacements")
        self.nb.AddPage(self.ModeTab, "Mode Shapes")
        
        self.Geometry(self.nb)  
        self.Forces(self.nb)
        self.Displacements(self.nb)
        self.Mode_shapes(self.nb)
        
#                btn = wx.Button(self, wx.ID_OK)
#        btn.SetDefault()
        
        self.buttonAccept = wx.Button(self.panel, wx.ID_OK,  label="Accept Design", size = (150,-1))
        self.buttonClose = wx.Button(self.panel, wx.ID_CANCEL,  label="Close", size = (150,-1))
        self.buttonAccept.SetDefault()
        self.hboxBTN = wx.BoxSizer(wx.HORIZONTAL)
        self.hboxBTN.Add(self.buttonClose)
        self.hboxBTN.Add(self.buttonAccept)
        
        self.vboxMP = wx.BoxSizer(wx.VERTICAL)
        self.vboxMP.AddSpacer(7)
        self.vboxMP.Add(self.hboxBTN, 0, wx.CENTER)
        self.vboxMP.Add(self.nb, 1, wx.LEFT | wx.TOP | wx.GROW)
        
        self.panel.SetSizer(self.vboxMP)
        self.vboxMP.Fit(self)
        
        self.OutputFrameSize = self.GetSize()

    def Geometry(self, parent):
        self.figG1 = Figure((4.0, 4.0), dpi=self.dpi, tight_layout = True, facecolor = 'white')
        self.figG2 = Figure((4.0, 4.0), dpi=self.dpi, tight_layout = True, facecolor = 'white')
        
        self.canvasG1 = FigCanvas(self.GeometryTab, -1, self.figG1)
        self.canvasG2 = FigCanvas(self.GeometryTab, -1, self.figG2)
        
        self.axesG1 = self.figG1.add_subplot(111, aspect='equal', frame_on = False)
        self.axesG1.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axesG1.set_ylim([self.Lim_Y1, self.Lim_Y2])  
        
        self.axesG2 = self.figG2.add_subplot(111, aspect='equal', frame_on = False)
        self.axesG2.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axesG2.set_ylim([self.Lim_Y1, self.Lim_Y2])
       
        self.cpoG = cpoG = wx.CollapsiblePane(self.GeometryTab, label="Data",style=wx.CP_DEFAULT_STYLE|wx.CP_NO_TLW_RESIZE)
        self.Bind(wx.EVT_COLLAPSIBLEPANE_CHANGED, self.OnPaneChangedG, cpoG)
        self.MakePaneContentG(cpoG.GetPane())
        self.cpoG.SetBackgroundColour("white")
        
#        self.vbox0G = wx.BoxSizer(wx.VERTICAL)
#        self.vbox0G.Add(self.canvasG1, 0, wx.TOP)
#        self.vbox0G.Add(self.list_ctrl11G, 0, wx.CENTER)
#        self.vbox0G.Add(self.list_ctrl21G, 0, wx.CENTER)
        
#        self.vbox1G = wx.BoxSizer(wx.VERTICAL)
#        self.vbox1G.Add(self.canvasG2, 0, wx.TOP)
#        self.vbox1G.Add(self.list_ctrl12G, 0, wx.CENTER)
#        self.vbox1G.Add(self.list_ctrl22G, 0, wx.CENTER)        
         
        self.hbox1G = wx.BoxSizer(wx.HORIZONTAL)
        self.hbox1G.Add(self.canvasG1, 0, wx.TOP)
        self.hbox1G.Add(self.canvasG2, 0, wx.TOP)
        
        self.vbox00G = wx.BoxSizer(wx.VERTICAL)
        self.vbox00G.Add(self.hbox1G, 0, wx.TOP)
        self.vbox00G.Add(self.cpoG, 0, wx.LEFT| wx.ALL, 15)
         
        self.GeometryTab.SetSizer(self.vbox00G)
        
        self.draw_by_scaleG(self.canvasG1,self.axesG1,self.NOD,False)
        self.draw_by_scaleG(self.canvasG2,self.axesG2,self.NOD_opt,True)
        
        self.axesG1.set_title('Initial Design')
        self.axesG1.set_xlabel('Total Mass = '+str(self.M_tot[0][0]))

        self.axesG2.set_title('Optimal Design')
        self.axesG2.set_xlabel('Total Mass = '+str(self.M_tot_opt[0][0]))

        self.canvasG1.draw()
        self.canvasG2.draw()
        
    def OnPaneChangedG(self, evt=None):
        # redo the layout
        dw, dh = wx.DisplaySize()
        w, h = self.OutputFrameSize
        ds = (dh - h)/2

        if self.cpoG.IsExpanded():
            self.SetSize((w, h+ds))
        else:
            self.SetSize((w, h))
            
        self.Layout()

    def MakePaneContentG(self, pane):
        self.list_ctrl11G = wx.ListCtrl(pane, size=(400,120),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl11G.InsertColumn(0, 'Node ID', width=100)
        self.list_ctrl11G.InsertColumn(1, 'X', width=100)
        self.list_ctrl11G.InsertColumn(2, 'Y', width=100)
        
        self.list_ctrl21G = wx.ListCtrl(pane, size=(400,120),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl21G.InsertColumn(0, 'Element ID', width=100)
        self.list_ctrl21G.InsertColumn(1, 'A', width=100)
        
        for i in range(0,len(self.NOD)):
            self.list_ctrl11G.InsertStringItem(i, str(int(self.NOD[i][0])))
            self.list_ctrl11G.SetStringItem(i, 1, str(self.NOD[i][1]))
            self.list_ctrl11G.SetStringItem(i, 2, str(self.NOD[i][2]))              

        for i in range(0,len(self.ELE)):
            self.list_ctrl21G.InsertStringItem(i, str(int(self.ELE[i][0])))
            self.list_ctrl21G.SetStringItem(i, 1, str(self.SEC[self.ELE[i][3]-1][1]))

        self.list_ctrl12G = wx.ListCtrl(pane, size=(400,120),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl12G.InsertColumn(0, 'Node ID', width=100)
        self.list_ctrl12G.InsertColumn(1, 'X', width=100)
        self.list_ctrl12G.InsertColumn(2, 'Y', width=100)
        
        self.list_ctrl22G = wx.ListCtrl(pane, size=(400,120),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl22G.InsertColumn(0, 'Element ID', width=100)
        self.list_ctrl22G.InsertColumn(1, 'A', width=100)
        
        for i in range(0,len(self.NOD)):
            self.list_ctrl12G.InsertStringItem(i, str(int(self.NOD_opt[i][0])))
            self.list_ctrl12G.SetStringItem(i, 1, str(self.NOD_opt[i][1]))
            self.list_ctrl12G.SetStringItem(i, 2, str(self.NOD_opt[i][2]))              

        for i in range(0,len(self.ELE)):
            self.list_ctrl22G.InsertStringItem(i, str(int(self.ELE_opt[i][0])))
            self.list_ctrl22G.SetStringItem(i, 1, str( self.ELE_opt[i][1] ))
        pane.SetBackgroundColour("white")
        
        self.vboxPG1 = wx.BoxSizer(wx.VERTICAL)
        self.vboxPG1.Add(self.list_ctrl11G, 0, wx.CENTER)
        self.vboxPG1.Add(self.list_ctrl21G, 0, wx.CENTER)
        
        self.vboxPG2 = wx.BoxSizer(wx.VERTICAL)
        self.vboxPG2.Add(self.list_ctrl12G, 0, wx.CENTER)
        self.vboxPG2.Add(self.list_ctrl22G, 0, wx.CENTER)        
         
        self.hboxPG = wx.BoxSizer(wx.HORIZONTAL)
        self.hboxPG.Add(self.vboxPG1, 0, wx.TOP)
        self.hboxPG.Add(self.vboxPG2, 0, wx.TOP)
        
        pane.SetSizer(self.hboxPG)
        
    def Forces(self, parent): 
        self.figF1 = Figure((4.0, 4.0), dpi=self.dpi, tight_layout = True, facecolor = 'white')
        self.figF2 = Figure((4.0, 4.0), dpi=self.dpi, tight_layout = True, facecolor = 'white')
        
        self.canvasF1 = FigCanvas(self.ForceTab, -1, self.figF1)
        self.canvasF2 = FigCanvas(self.ForceTab, -1, self.figF2)
        
        self.axesF1 = self.figF1.add_subplot(111, aspect='equal', frame_on = False, title='sdv')
        self.axesF1.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axesF1.set_ylim([self.Lim_Y1, self.Lim_Y2])  
        
        self.axesF2 = self.figF2.add_subplot(111, aspect='equal', frame_on = False)
        self.axesF2.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axesF2.set_ylim([self.Lim_Y1, self.Lim_Y2]) 
        
        self.cpoF = cpoF = wx.CollapsiblePane(self.ForceTab, label="Data",style=wx.CP_DEFAULT_STYLE|wx.CP_NO_TLW_RESIZE)
        self.Bind(wx.EVT_COLLAPSIBLEPANE_CHANGED, self.OnPaneChangedF, cpoF)
        self.MakePaneContentF(cpoF.GetPane())
        self.cpoF.SetBackgroundColour("white")

        self.hbox1F = wx.BoxSizer(wx.HORIZONTAL)
        self.hbox1F.Add(self.canvasF1, 0, wx.TOP)
        self.hbox1F.Add(self.canvasF2, 0, wx.TOP)
        
        self.vbox00F = wx.BoxSizer(wx.VERTICAL)
        self.vbox00F.Add(self.hbox1F, 0, wx.TOP)
        self.vbox00F.Add(self.cpoF, 0, wx.LEFT| wx.ALL, 15)
         
        self.ForceTab.SetSizer(self.vbox00F)
         
        self.draw_by_scaleF(self.canvasF1,self.axesF1,self.NOD,False)
        self.draw_by_scaleF(self.canvasF2,self.axesF2,self.NOD_opt,True)
        
        self.axesF1.set_title('Initial Design')
        self.axesF1.set_xlabel('Maximum Stress = '+str(self.S_max[0][0]))
        
        self.axesF2.set_title('Optimal Design')
        self.axesF2.set_xlabel('Maximum Stress = '+str(self.S_max_opt[0][0]))
        
        self.draw_reactions(self.axesF1,self.NOD,self.P_nodal,self.list_ctrl11F)
        self.draw_reactions(self.axesF2,self.NOD_opt,self.P_nodal_opt,self.list_ctrl12F)

        self.canvasF1.draw()
        self.canvasF2.draw()
        
    def OnPaneChangedF(self, evt=None):
        # redo the layout
        dw, dh = wx.DisplaySize()
        w, h = self.OutputFrameSize
        ds = (dh - h)/2

        if self.cpoG.IsExpanded():
            self.SetSize((w, h+ds))
        else:
            self.SetSize((w, h))
            
        self.Layout()

    def MakePaneContentF(self, pane):
        self.list_ctrl11F = wx.ListCtrl(pane, size=(400,120),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl11F.InsertColumn(0, 'Node ID', width=100)
        self.list_ctrl11F.InsertColumn(1, 'Fx', width=100)
        self.list_ctrl11F.InsertColumn(2, 'Fy', width=100)
        
        self.list_ctrl21F = wx.ListCtrl(pane, size=(400,120),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl21F.InsertColumn(0, 'Element ID', width=100)
        self.list_ctrl21F.InsertColumn(1, 'F', width=100)

        for i in range(0,len(self.NOD)):
            self.list_ctrl11F.InsertStringItem(i, str(int(self.P_nodal[i][0])))
            self.list_ctrl11F.SetStringItem(i, 1, str(self.P_nodal[i][1]))
            self.list_ctrl11F.SetStringItem(i, 2, str(self.P_nodal[i][2]))              

        for i in range(0,len(self.ELE)):
            self.list_ctrl21F.InsertStringItem(i, str(int(self.P_element[i][0])))
            self.list_ctrl21F.SetStringItem(i, 1, str(self.P_element[i][1]))

        self.list_ctrl12F = wx.ListCtrl(pane, size=(400,120),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl12F.InsertColumn(0, 'Node ID', width=100)
        self.list_ctrl12F.InsertColumn(1, 'Fx', width=100)
        self.list_ctrl12F.InsertColumn(2, 'Fy', width=100)
        
        self.list_ctrl22F = wx.ListCtrl(pane, size=(400,120),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl22F.InsertColumn(0, 'Element ID', width=100)
        self.list_ctrl22F.InsertColumn(1, 'F', width=100)
        
        for i in range(0,len(self.NOD)):
            self.list_ctrl12F.InsertStringItem(i, str(int(self.P_nodal_opt[i][0])))
            self.list_ctrl12F.SetStringItem(i, 1, str(self.P_nodal_opt[i][1]))
            self.list_ctrl12F.SetStringItem(i, 2, str(self.P_nodal_opt[i][2]))              

        for i in range(0,len(self.ELE)):
            self.list_ctrl22F.InsertStringItem(i, str(int(self.P_element_opt[i][0])))
            self.list_ctrl22F.SetStringItem(i, 1, str(self.P_element_opt[i][1]))
        pane.SetBackgroundColour("white")
        
        self.vboxPF1 = wx.BoxSizer(wx.VERTICAL)
        self.vboxPF1.Add(self.list_ctrl11F, 0, wx.CENTER)
        self.vboxPF1.Add(self.list_ctrl21F, 0, wx.CENTER)
        
        self.vboxPF2 = wx.BoxSizer(wx.VERTICAL)
        self.vboxPF2.Add(self.list_ctrl12F, 0, wx.CENTER)
        self.vboxPF2.Add(self.list_ctrl22F, 0, wx.CENTER)        
         
        self.hboxPF = wx.BoxSizer(wx.HORIZONTAL)
        self.hboxPF.Add(self.vboxPF1, 0, wx.TOP)
        self.hboxPF.Add(self.vboxPF2, 0, wx.TOP)
        
        pane.SetSizer(self.hboxPF)
            

    def draw_reactions(self,ax,NOD,P_nodal,list_):
        R_forces = []
        Nod_R = []
        for i in range(0,len(self.BC[:,0])):
            j = self.BC[i,0] - 1
            Nod_R.append(j+1)
            if len(self.FOR1) != 0:
                if len(self.FOR1) != 1 or self.FOR1[0][0] == 0:
                    if j+1 in self.FOR1[:,0]:
                        for k in range(0,len(self.FOR1[:,0])):
                            if j+1 == self.FOR1[k,0]:
                                R_forces.append([P_nodal[k][1]-self.FOR1[k][1], 
                                                 P_nodal[k][2]-self.FOR1[k][2]])

                    else:    
                        R_forces.append([P_nodal[j][1], P_nodal[j][2]])
                                
                else:    
                    R_forces.append([P_nodal[j][1], P_nodal[j][2]])
                            
            else:    
                R_forces.append([P_nodal[j][1], P_nodal[j][2]])

            if self.BC[i][1] == 0:
                R_forces[i][0] = 0
                 
            if self.BC[i][2] == 0:
                R_forces[i][1] = 0 

        max_val = 0
        for i in range(0,len(Nod_R)):
            for j in range(0,2):
                if abs(R_forces[i][j]) > max_val:
                    max_val = abs(R_forces[i][j])
        
        if max_val == 0:
            max_val = 1

        toler = max_val/1000
        scale = self.Dim_X/5/max_val
        for i in range(0,len(Nod_R)):
            p =[NOD[Nod_R[i]-1,1], NOD[Nod_R[i]-1,2]]
            if abs(R_forces[i][0]) > toler:
                sign = R_forces[i][0]/abs(R_forces[i][0])
                xyT = (p[0] + sign*self.Dim_X/20 + R_forces[i][0]*scale, p[1])
                ax.annotate('',     xy = (p[0], p[1]),
                                   xytext     = xyT,
                                   arrowprops = dict(arrowstyle="<-",alpha=0.7, color='b'))
   
                ax.text(xyT[0]+self.Dim_X/50,xyT[1]+self.Dim_X/100,
                                    'R'+str(int(Nod_R[i]))+'x', size='small',color='b',alpha=0.7 )
                list_.SetStringItem(int(Nod_R[i]-1), 1, 'R'+str(int(Nod_R[i]))+'x = '+str(P_nodal[int(Nod_R[i]-1)][1]))
                  
            if abs(R_forces[i][1]) > toler:
                sign = R_forces[i][1]/abs(R_forces[i][1])
                xyT = (p[0], p[1] + sign*self.Dim_X/20 + R_forces[i][1]*scale)
                ax.annotate('',     xy = (p[0], p[1]),
                                   xytext     = xyT,
                                   arrowprops = dict(arrowstyle="<-",alpha=0.7, color='b'))
   
                ax.text(xyT[0]+self.Dim_X/100,xyT[1]+self.Dim_X/100,
                                    'R'+str(int(Nod_R[i]))+'y', size='small',color='b',alpha=0.7 )
                list_.SetStringItem(int(Nod_R[i]-1), 2, 'R'+str(int(Nod_R[i]))+'y = '+str(P_nodal[int(Nod_R[i]-1)][2]))
    
    def Displacements(self, parent): 
        self.figD1 = Figure((4.0, 4.0), dpi=self.dpi, tight_layout = True, facecolor = 'white')
        self.figD2 = Figure((4.0, 4.0), dpi=self.dpi, tight_layout = True, facecolor = 'white')
        
        self.canvasD1 = FigCanvas(self.DisplacementTab, -1, self.figD1)
        self.canvasD2 = FigCanvas(self.DisplacementTab, -1, self.figD2)
        
        self.axesD1 = self.figD1.add_subplot(111, aspect='equal', frame_on = False, title='sdv')
        self.axesD1.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axesD1.set_ylim([self.Lim_Y1, self.Lim_Y2])  
        
        self.axesD2 = self.figD2.add_subplot(111, aspect='equal', frame_on = False)
        self.axesD2.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axesD2.set_ylim([self.Lim_Y1, self.Lim_Y2]) 

        self.cpoD = cpoD = wx.CollapsiblePane(self.DisplacementTab, label="Data",style=wx.CP_DEFAULT_STYLE|wx.CP_NO_TLW_RESIZE)
        self.Bind(wx.EVT_COLLAPSIBLEPANE_CHANGED, self.OnPaneChangedD, cpoD)
        self.MakePaneContentD(cpoD.GetPane())
        self.cpoD.SetBackgroundColour("white")

        self.hbox1D = wx.BoxSizer(wx.HORIZONTAL)
        self.hbox1D.Add(self.canvasD1, 0, wx.TOP)
        self.hbox1D.Add(self.canvasD2, 0, wx.TOP)
        
        self.vbox00D = wx.BoxSizer(wx.VERTICAL)
        self.vbox00D.Add(self.hbox1D, 0, wx.TOP)
        self.vbox00D.Add(self.cpoD, 0, wx.LEFT| wx.ALL, 15)             
        
#        self.vbox0D = wx.BoxSizer(wx.VERTICAL)
#        self.vbox0D.Add(self.canvasD1, 0, wx.TOP)
#        self.vbox0D.Add(self.list_ctrl11D, 0, wx.CENTER)
#        
#        self.vbox1D = wx.BoxSizer(wx.VERTICAL)
#        self.vbox1D.Add(self.canvasD2, 0, wx.TOP)
#        self.vbox1D.Add(self.list_ctrl12D, 0, wx.CENTER)  
#         
#        self.hbox1D = wx.BoxSizer(wx.HORIZONTAL)
#        self.hbox1D.Add(self.vbox0D, 0, wx.TOP)
#        self.hbox1D.Add(self.vbox1D, 0, wx.TOP)
         
        self.DisplacementTab.SetSizer(self.vbox00D)
         
        self.draw_by_scaleD(self.canvasD1,self.axesD1,self.NOD,self.D_nodal,False)
        self.draw_by_scaleD(self.canvasD2,self.axesD2,self.NOD_opt,self.D_nodal_opt,True)
        
        self.axesD1.set_title('Initial Design')
        self.axesD1.set_xlabel('External Work = '+str(self.W_ext[0][0]))
        
        self.axesD2.set_title('Optimal Design')
        self.axesD2.set_xlabel('External Work = '+str(self.W_ext_opt[0][0]))

        self.canvasD1.draw()
        self.canvasD2.draw()
        
    def OnPaneChangedD(self, evt=None):
        # redo the layout
        dw, dh = wx.DisplaySize()
        w, h = self.OutputFrameSize
        ds = (dh - h)/2

        if self.cpoG.IsExpanded():
            self.SetSize((w, h+ds))
        else:
            self.SetSize((w, h))
            
        self.Layout()

    def MakePaneContentD(self, pane):
        self.list_ctrl11D = wx.ListCtrl(pane, size=(400,220),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl11D.InsertColumn(0, 'Node ID', width=100)
        self.list_ctrl11D.InsertColumn(1, 'dx', width=100)
        self.list_ctrl11D.InsertColumn(2, 'dy', width=100)
        
        for i in range(0,len(self.D_nodal)):
            self.list_ctrl11D.InsertStringItem(i, str(int(self.D_nodal[i][0])))
            self.list_ctrl11D.SetStringItem(i, 1, str(self.D_nodal[i][1]))
            self.list_ctrl11D.SetStringItem(i, 2, str(self.D_nodal[i][2]))              

        self.list_ctrl12D = wx.ListCtrl(pane, size=(400,220),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl12D.InsertColumn(0, 'Node ID', width=100)
        self.list_ctrl12D.InsertColumn(1, 'dx', width=100)
        self.list_ctrl12D.InsertColumn(2, 'dy', width=100)
        
        for i in range(0,len(self.D_nodal)):
            self.list_ctrl12D.InsertStringItem(i, str(int(self.D_nodal_opt[i][0])))
            self.list_ctrl12D.SetStringItem(i, 1, str(self.D_nodal_opt[i][1]))
            self.list_ctrl12D.SetStringItem(i, 2, str(self.D_nodal_opt[i][2])) 
        pane.SetBackgroundColour("white")
        
        self.vboxPD1 = wx.BoxSizer(wx.VERTICAL)
        self.vboxPD1.Add(self.list_ctrl11D, 0, wx.CENTER)
        
        self.vboxPD2 = wx.BoxSizer(wx.VERTICAL)
        self.vboxPD2.Add(self.list_ctrl12D, 0, wx.CENTER)      
         
        self.hboxPD = wx.BoxSizer(wx.HORIZONTAL)
        self.hboxPD.Add(self.vboxPD1, 0, wx.TOP)
        self.hboxPD.Add(self.vboxPD2, 0, wx.TOP)
        
        pane.SetSizer(self.hboxPD)
            
    
    def Mode_shapes(self, parent): 
        suma = 0
        for i in range(0,len(self.BC)):
            suma = suma + sum(self.BC[i][1:4])
        
        self.n_modes = len(self.NOD)*2 - suma
        self.Mode = 1
        
        self.figM1 = Figure((4.0, 4.0), dpi=self.dpi, tight_layout = True, facecolor = 'white')
        self.figM2 = Figure((4.0, 4.0), dpi=self.dpi, tight_layout = True, facecolor = 'white')
        
        self.canvasM1 = FigCanvas(self.ModeTab, -1, self.figM1)
        self.canvasM2 = FigCanvas(self.ModeTab, -1, self.figM2)
        
        self.axesM1 = self.figM1.add_subplot(111, aspect='equal', frame_on = False, title='sdv')
        self.axesM1.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axesM1.set_ylim([self.Lim_Y1, self.Lim_Y2])  
        
        self.axesM2 = self.figM2.add_subplot(111, aspect='equal', frame_on = False)
        self.axesM2.set_xlim([self.Lim_X1, self.Lim_X2])
        self.axesM2.set_ylim([self.Lim_Y1, self.Lim_Y2]) 

        self.cpoM = cpoM = wx.CollapsiblePane(self.ModeTab, label="Data",style=wx.CP_DEFAULT_STYLE|wx.CP_NO_TLW_RESIZE)
        self.Bind(wx.EVT_COLLAPSIBLEPANE_CHANGED, self.OnPaneChangedM, cpoM)
        self.MakePaneContentD(cpoM.GetPane())
        self.cpoM.SetBackgroundColour("white")

        self.hbox1M = wx.BoxSizer(wx.HORIZONTAL)
        self.hbox1M.Add(self.canvasM1, 0, wx.TOP)
        self.hbox1M.Add(self.canvasM2, 0, wx.TOP)
        
        
#        self.vbox0M = wx.BoxSizer(wx.VERTICAL)
#        self.vbox0M.Add(self.canvasM1, 0, wx.CENTER)
#        self.vbox0M.Add(self.list_ctrl11M, 0, wx.CENTER)
#                
#        self.vbox1M = wx.BoxSizer(wx.VERTICAL)
#        self.vbox1M.Add(self.canvasM2, 0, wx.CENTER)
#        self.vbox1M.Add(self.list_ctrl12M, 0, wx.CENTER)       
#         
#        self.hbox1M = wx.BoxSizer(wx.HORIZONTAL)
#        self.hbox1M.Add(self.vbox0M, 0, wx.CENTER)
#        self.hbox1M.Add(self.vbox1M, 0, wx.CENTER)
        
        self.spin = wx.SpinCtrl(self.ModeTab, -1, '', size=(80,-1))
        self.spin.SetRange(1,self.n_modes)
        self.spin.SetValue(1)
#        self.Bind(wx.EVT_TEXT, self.EvtMode, self.spin)
        self.Bind(wx.EVT_TEXT, lambda event:self.EvtMode(event,cpoM.GetPane()), self.spin)
        
        self.hbox000M = wx.BoxSizer(wx.HORIZONTAL)
        self.hbox000M.Add(wx.StaticText(self.ModeTab, label="Mode"), 1, wx.CENTRE)
        self.hbox000M.Add(self.spin, 1, wx.CENTRE)
        
        self.vbox00M = wx.BoxSizer(wx.VERTICAL)
        self.vbox00M.Add(self.hbox1M, 0, wx.TOP)
        self.vbox00M.Add(self.hbox000M, 0, wx.TOP)
        self.vbox00M.Add(self.cpoM, 0, wx.LEFT| wx.ALL, 15) 


        self.ModeTab.SetSizer(self.vbox00M)
         
        self.draw_by_scaleM(self.canvasM1,self.axesM1,self.NOD,self.D_nodal,False,self.E_vect,1)
        self.draw_by_scaleM(self.canvasM2,self.axesM2,self.NOD_opt,self.D_nodal_opt,True,self.E_vect_opt,1)
        
        self.axesM1.set_title('Initial Design')
        self.axesM1.set_xlabel('eigen-frequency = '+str(self.E_val[0][0]))
        
        self.axesM2.set_title('Optimal Design')
        self.axesM2.set_xlabel('eigen-frequency = '+str(self.E_val_opt[0][0]))

        self.canvasM1.draw()
        self.canvasM2.draw()
        
    def OnPaneChangedM(self, evt=None):
        # redo the layout
        dw, dh = wx.DisplaySize()
        w, h = self.OutputFrameSize
        ds = (dh - h)/2

        if self.cpoG.IsExpanded():
            self.SetSize((w, h+ds))
        else:
            self.SetSize((w, h))
            
        self.Layout()

    def MakePaneContentM(self, pane):
        self.list_ctrl11M = wx.ListCtrl(pane, size=(320,180),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl11M.InsertColumn(0, 'Node ID', width=100)
        self.list_ctrl11M.InsertColumn(1, 'dx', width=100)
        self.list_ctrl11M.InsertColumn(2, 'dy', width=100)
        
        for i in range(0,len(self.D_nodal)):
            self.list_ctrl11M.InsertStringItem(i, str(i+1))
            self.list_ctrl11M.SetStringItem(i, 1, str(self.E_vect[2*i,self.Mode-1]))
            self.list_ctrl11M.SetStringItem(i, 2, str(self.E_vect[2*i+1,self.Mode-1]))              

        self.list_ctrl12M = wx.ListCtrl(pane, size=(320,180),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl12M.InsertColumn(0, 'Node ID', width=100)
        self.list_ctrl12M.InsertColumn(1, 'dx', width=100)
        self.list_ctrl12M.InsertColumn(2, 'dy', width=100)
        
        for i in range(0,len(self.D_nodal)):
            self.list_ctrl12M.InsertStringItem(i, str(i+1))
            self.list_ctrl12M.SetStringItem(i, 1, str(self.E_vect_opt[2*i,self.Mode-1]))
            self.list_ctrl12M.SetStringItem(i, 2, str(self.E_vect_opt[2*i+1,self.Mode-1]))  
        self.vboxPM1 = wx.BoxSizer(wx.VERTICAL)
        self.vboxPM1.Add(self.list_ctrl11M, 0, wx.CENTER)
        
        self.vboxPM2 = wx.BoxSizer(wx.VERTICAL)
        self.vboxPM2.Add(self.list_ctrl12M, 0, wx.CENTER)      
         
        self.hboxPM = wx.BoxSizer(wx.HORIZONTAL)
        self.hboxPM.Add(self.vboxPM1, 0, wx.TOP)
        self.hboxPM.Add(self.vboxPM2, 0, wx.TOP)
        
        pane.SetSizer(self.hboxPM)

    def EvtMode(self,event,pane):
        Md = int(self.spin.GetValue())
        self.Mode = Md
        print(Md)
#        self.list_ctrl11M.DeleteAllItems()
#        self.list_ctrl12M.DeleteAllItems()
#        for i in range(0,len(self.D_nodal)):
#            self.list_ctrl11M.InsertStringItem(i, str(i+1))
#            self.list_ctrl11M.SetStringItem(i, 1, str(self.E_vect[2*i,Md-1]))
#            self.list_ctrl11M.SetStringItem(i, 2, str(self.E_vect[2*i+1,Md-1]))              
#        
#        for i in range(0,len(self.D_nodal)):
#            self.list_ctrl12M.InsertStringItem(i, str(i+1))
#            self.list_ctrl12M.SetStringItem(i, 1, str(self.E_vect_opt[2*i,Md-1]))
#            self.list_ctrl12M.SetStringItem(i, 2, str(self.E_vect_opt[2*i+1,Md-1]))
        
        self.MakePaneContentD(self.cpoM.GetPane())
            
        self.draw_by_scaleM(self.canvasM1,self.axesM1,self.NOD,self.D_nodal,False,self.E_vect,Md)
        self.draw_by_scaleM(self.canvasM2,self.axesM2,self.NOD_opt,self.D_nodal_opt,True,self.E_vect_opt,Md)
        self.axesM1.set_xlabel('eigen-frequency = '+str(self.E_val[0][Md-1]))
        self.axesM2.set_xlabel('eigen-frequency = '+str(self.E_val_opt[0][Md-1]))
        self.axesM1.set_title('Initial Design')
        self.axesM2.set_title('Optimal Design')
        self.canvasM1.draw()
        self.canvasM2.draw()

    def EvtSc(self,event):
        self.Scale_factor = float(event.GetString())
        self.draw_by_scale()
 
    def draw_by_scaleG(self,canvas,ax,NOD,opt_):
        self.draw_figure(NOD,'black',True,canvas,ax,True,True,True,False,opt_)
        
    def draw_by_scaleF(self,canvas,ax,NOD,opt_):
        self.draw_figure(NOD,'black',True,canvas,ax,True,False,True,True,opt_)

    def draw_by_scaleD(self,canvas,ax,NOD,D_nodal,opt_):
        NOD_D = deepcopy(NOD)
        for i in range(0,len(self.NOD[:,0])):
            NOD_D[i][1] = NOD_D[i][1] + self.Scale_factor*float(D_nodal[i][1])
            NOD_D[i][2] = NOD_D[i][2] + self.Scale_factor*float(D_nodal[i][2])
            
        self.draw_figure(NOD,'Grey',True,canvas,ax,True,True,True,False,opt_)
        self.draw_figure(NOD_D,'black',False,canvas,ax,True,True,True,False,opt_)

    def draw_by_scaleM(self,canvas,ax,NOD,D_nodal,opt_,E_vect,N):
        NOD_D = deepcopy(NOD)
        for i in range(0,len(self.NOD[:,0])):
            NOD_D[i][1] = NOD_D[i][1] + (self.Lim_X2-self.Lim_X1)/30*float(E_vect[2*i,N-1])
            NOD_D[i][2] = NOD_D[i][2] + (self.Lim_X2-self.Lim_X1)/30*(E_vect[2*i+1,N-1])
            
        self.draw_figure(NOD,'Grey',True,canvas,ax,True,True,True,False,opt_)
        self.draw_figure(NOD_D,'black',False,canvas,ax,True,True,True,False,opt_)

    def get_element_properties(self, ind, props):
        x1, y1 = self.NOD[self.ELE[ind][1]-1][1], self.NOD[self.ELE[ind][1]-1][2]
        x2, y2 = self.NOD[self.ELE[ind][2]-1][1], self.NOD[self.ELE[ind][2]-1][2]
          
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
 
    def draw_figure(self,NOD,colr,clear,canv,ax,cb_grid,plot_bc, plot_bars, plot_labs, opt_):
        """ Redraws the figure
        """
        # clear the axes and redraw the plot anew
        #
        if clear == True:
            ax.clear()
            ax.set_xlim([self.Lim_X1, self.Lim_X2])
            ax.set_ylim([self.Lim_Y1, self.Lim_Y2])       
#             ax.grid(cb_grid.IsChecked())
            ax.grid(cb_grid)
 
         
        for i in range(0,len(NOD[:,0])):
            if NOD[i,0] != 0:
                ax.scatter(NOD[i][1], NOD[i][2], color = 'black', s=1)
                dist = float(self.Dim_X)/float(60)
                if plot_labs == True:
                    ax.text(NOD[i][1]+dist,NOD[i][2]+dist, 
                                   str(int(NOD[i][0])), size='small',color='k')
         
        if plot_bars == True:
            for i in range(0,len(self.ELE[:,0])):
                if opt_ == True:
                    wdth = np.sqrt((self.ELE_opt[i][1])/np.pi)
                else:
                    wdth = np.sqrt((self.SEC[self.ELE[i,3]-1,1])/np.pi)
                wdth=wdth*5
                if (self.ELE[i,1] != 0) and (self.ELE[i,2] != 0):
                    X1, X2 = NOD[self.ELE[i,1]-1,1], NOD[self.ELE[i,2]-1,1]
                    Y1, Y2 = NOD[self.ELE[i,1]-1,2], NOD[self.ELE[i,2]-1,2]
                    ax.plot((X1,X2), (Y1,Y2), color = colr, linewidth=wdth)
                    dist = float(self.Dim_X)/float(60)
                    p = ((X2+X1)/2-2*dist,(Y2+Y1)/2+dist)
                if plot_labs == True:
                    ax.text(p[0],p[1], 
                                   'E'+str(int(self.ELE[i][0])), size='small',color='k',alpha=0.5)
                    
        if plot_bc == True:    
            for i in range(0,len(self.BC[:,0])):
                p = [NOD[self.BC[i,0]-1,1], NOD[self.BC[i,0]-1,2]]
                if self.BC[i,0] != 0:
                    if (self.BC[i,1] == 1) and (self.BC[i,2] == 1):
                        self.plot_hinge(p,0.03*self.Dim_X,self.BC[i,3],colr,ax)
                    else:
                        print(self.BC)
                        if (self.BC[i,1] == 0) and (self.BC[i,2] == 1):
                            print('HEYHEYEHEYEHEYHEY1')
                            self.plot_roller(p,0.03*self.Dim_X,self.BC[i,3],colr,ax)
                        elif (self.BC[i,1] == 1) and (self.BC[i,2] == 0):
                            print('HEYHEYEHEYEHEYHEY2')
                            self.plot_roller(p,0.03*self.Dim_X,self.BC[i,3]+90,colr,ax)
 
 
        canv.draw()
 
    def plot_hinge(self, position, scale, angle, colr, ax):
        pi = 3.14159265359
         
        verts = [(-1.25, -0.75), (-1.0, -0.75), (-0.5 , -1.05), (-1.0, -0.75), (-0.5, -0.75), 
                 (0.0  , -1.05), (-0.5, -0.75), (0.0  , -0.75), (0.5 , -1.05), (0.0, -0.75 ), 
                 (0.5  , -0.75), (1.0 , -1.05), (0.5  , -0.75), (1.0 , -0.75), (1.5, -1.05 ), 
                 (1.0  , -0.75), (1.5 , -0.75), (-0.75, -0.75), (0.0 , 0.0  ), (0.75, -0.75)]
         
         
        codes = [Path.MOVETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO]
         
        angle = pi*angle/180
        vertsT = verts[:]
        for i in range(0,len(verts)):
            vertsT[i] = (verts[i][0]*np.cos(angle)-verts[i][1]*np.sin(angle), verts[i][0]*np.sin(angle)+verts[i][1]*np.cos(angle))
 
        for i in range(0,len(verts)):
            vertsT[i] = (scale*vertsT[i][0],scale*vertsT[i][1])
     
        for i in range(0,len(verts)):
            vertsT[i] = (vertsT[i][0] + position[0],vertsT[i][1] + position[1])
      
        pathT = Path(vertsT, codes)
 
        hingeT = patches.PathPatch(pathT, edgecolor = colr, facecolor='none', lw=1)
         
        ax.add_patch(hingeT)
 
    def plot_roller(self, position, scale, angle, colr, ax):
        pi = 3.14159265359
         
        verts = [(-1.25, -1.  ), (-1.0 , -1.  ), (-0.5 , -1.30), (-1.0 , -1.  ), (-0.5, -1. ), 
                 (0.0  , -1.30), (-0.5 , -1.  ), (0.0  , -1.  ), (0.5  , -1.30), (0.0, -1.  ), 
                 (0.5  , -1.  ), (1.0  , -1.30), (0.5  , -1.  ), (1.0  , -1.  ), (1.5, -1.30), 
                 (1.0  , -1.  ), (1.5  , -1.  ), (-0.75, -0.75), (-0.75, -0.75), (0.0, 0.0  ), 
                 (0.75 , -0.75), (-0.75, -0.75)]
     
        codes = [Path.MOVETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, 
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO, Path.MOVETO, Path.LINETO, Path.LINETO,
                 Path.LINETO, Path.LINETO,]
         
        angle = pi*angle/180
        vertsT = verts[:]
        for i in range(0,len(verts)):
            vertsT[i] = (verts[i][0]*np.cos(angle)-verts[i][1]*np.sin(angle), verts[i][0]*np.sin(angle)+verts[i][1]*np.cos(angle))
 
        for i in range(0,len(verts)):
            vertsT[i] = (scale*vertsT[i][0],scale*vertsT[i][1])
     
        for i in range(0,len(verts)):
            vertsT[i] = (vertsT[i][0] + position[0],vertsT[i][1] + position[1])
      
        pathT = Path(vertsT, codes)
 
        hingeT = patches.PathPatch(pathT, edgecolor = colr, facecolor='none', lw=1)
         
        ax.add_patch(hingeT)
 
 
    def on_pick(self, event):
        box_points = event.artist.get_bbox().get_points()
        msg = "You've clicked on a bar with coords:\n %s" % box_points
         
        dlg = wx.MessageDialog(
            self, 
            msg, 
            "Click!",
            wx.OK | wx.ICON_INFORMATION)
 
        dlg.ShowModal() 
        dlg.Destroy()        
 
    def on_cb_gridD(self, event):
        self.axesD.grid(self.cb_gridD.IsChecked())
        self.canvasD.draw()
         
    def on_cb_gridR(self, event):
        self.axesR.grid(self.cb_gridR.IsChecked())
        self.canvasR.draw()
 
    def on_cb_gridA(self, event):
        self.axesA.grid(self.cb_gridA.IsChecked())
        self.canvasA.draw()
         
    def on_cb_gridS(self, event):
        self.axesS.grid(self.cb_gridS.IsChecked())
        self.canvasS.draw()
         
    def on_cb_gridM(self, event):
        self.axesM.grid(self.cb_gridM.IsChecked())
        self.canvasM.draw()
         
    def on_cb_gridMS(self, event):
        self.axesMS.grid(self.cb_gridMS.IsChecked())
        self.canvasMS.draw()