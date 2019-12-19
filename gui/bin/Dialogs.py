import wx
import wx.grid
import numpy as np
# from blaze.expr.table import selection


class DimDialog(wx.Dialog):
    def __init__(self,x1,y1,x2,y2,GS):
        title = 'Dimensions'
        
        self.Lim_X1 = x1
        self.Lim_Y1 = y1
        self.Lim_X2 = x2
        self.Lim_Y2 = y2
        
        self.Grid_spacing = GS
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)

        sizer = wx.BoxSizer(wx.VERTICAL)

        label = wx.StaticText(self, -1, "Set Plot Limits")
        label.SetHelpText("Assign the approximate dimensions of the problem. These dimensions should be larger that the dimensions of the frame in order to display the plots appropriately")
        sizer.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        
        gridSizer = wx.GridBagSizer(5, 5)

        gridSizer.Add(wx.StaticText(self, -1, "Lower"), (0,1))
        gridSizer.Add(wx.StaticText(self, -1, "Upper"), (0,2))
        gridSizer.Add(wx.StaticText(self, -1, "X axis: "), (1,0))

        text11 = wx.TextCtrl(self, -1, str(self.Lim_X1), size=(80,-1))
        text11.SetHelpText("This is the lower limit of the X axis.")
        self.Bind(wx.EVT_TEXT, self.EvtX1, text11)
        gridSizer.Add(text11, (1,1))

        text12 = wx.TextCtrl(self, -1, str(self.Lim_X2), size=(80,-1))
        text12.SetHelpText("This is the upper limit of the X axis.")
        self.Bind(wx.EVT_TEXT, self.EvtX2, text12)
        gridSizer.Add(text12, (1,2))

        gridSizer.Add(wx.StaticText(self, -1, "Y axis: "), (2,0))

        text21 = wx.TextCtrl(self, -1, str(self.Lim_Y1), size=(80,-1))
        text21.SetHelpText("This is the lower limit of the Y axis.")
        self.Bind(wx.EVT_TEXT, self.EvtY1, text21)
        gridSizer.Add(text21, (2,1))

        text22 = wx.TextCtrl(self, -1, str(self.Lim_Y2), size=(80,-1))
        text22.SetHelpText("This is the upper limit of the Y axis.")
        self.Bind(wx.EVT_TEXT, self.EvtY2, text22)
        gridSizer.Add(text22, (2,2))

        sizer.Add(gridSizer, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        hsizer = wx.BoxSizer(wx.HORIZONTAL)
        
        hsizer.Add(wx.StaticText(self, -1, "Grid Spacing: "), 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5) 
        textGS = wx.TextCtrl(self, -1, str(self.Grid_spacing), size=(80,-1))
        self.Bind(wx.EVT_TEXT, self.EvtGS, textGS)
        hsizer.Add(textGS, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5) 

        sizer.Add(hsizer, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def EvtX1(self, event):
        self.Lim_X1 = float(event.GetString())
 
    def EvtY1(self, event):
        self.Lim_Y1 = float(event.GetString())
        
    def EvtX2(self, event):
        self.Lim_X2 = float(event.GetString())
        
    def EvtY2(self, event):
        self.Lim_Y2 = float(event.GetString())
        
    def EvtGS(self, event):
        self.Grid_spacing = float(event.GetString())
        
class NodalForce(wx.Dialog):
    def __init__(self,FOR,PATH):
        title = 'Nodal Forces'
        
        self.FOR = FOR
        self.PATH = PATH
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)


        sizer = wx.BoxSizer(wx.VERTICAL)

        self.gridForces1 = wx.grid.Grid(self, size=(350,150))
        
        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.FGridUpdate)
        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.FGridRight)
        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.FGridRight)
        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowF)
        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowF)            
        
        if len(self.FOR) != 0:
            self.gridForces1.CreateGrid(len(self.FOR),4)
        else:
            self.gridForces1.CreateGrid(0,4)
                                        
        self.gridForces1.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
        self.gridForces1.SetRowLabelSize(3)
        self.gridForces1.SetColLabelValue(0, 'Node ID')
        self.gridForces1.SetColLabelValue(1, 'X force')
        self.gridForces1.SetColLabelValue(2, 'Y force')
        self.gridForces1.SetColLabelValue(3, 'Moment')
#         self.gridForces1.SetColSize(1, 80)
#         self.gridForces1.SetColSize(2, 120)
        
        if len(self.FOR) != 0:
            for i in range(0,len(self.FOR)):
                    if self.FOR[i][0] != 0:
                        self.gridForces1.SetCellValue(i,0,str(int(self.FOR[i][0])))
                    for j in range(1,len(self.FOR[0])):
                        if self.FOR[i][j] != 0:
                            self.gridForces1.SetCellValue(i,j,str(self.FOR[i][j]))

        sizer.Add(self.gridForces1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.buttonF1 = wx.Button(self, label="Add")
        self.Bind(wx.EVT_BUTTON, self.OnClickF, self.buttonF1)
        
        self.buttonDelF1 = wx.Button(self, label="Delete")
        self.Bind(wx.EVT_BUTTON, self.ClickDelFor, self.buttonDelF1)

        hbox = wx.BoxSizer(wx.HORIZONTAL)
        hbox.Add(self.buttonF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        hbox.Add(self.buttonDelF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)


#         Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
        Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())

        sizer.Add(hbox, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        sizer.Add(Img, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def ActiveRowF(self,event):
        self.active_rowF = event.GetRow()
        event.Skip()

    def ClickDelFor(self,event):
        self.For_to_del = self.active_rowF
        self.OnDelFor(0)

    def FGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        Value = float(self.gridForces1.GetCellValue(R, C))
        self.FOR[R][C] = Value
            
    def FGridRight(self,event):
        self.For_to_del = event.GetRow()
        FRMenu = wx.Menu()
        item = wx.MenuItem(FRMenu, wx.NewId(), "Delete Force")
        FRMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelFor, item)
        self.PopupMenu(FRMenu)
        
    def OnClickF(self,event):
        self.gridForces1.AppendRows(1, updateLabels = True)
        self.FOR.append([0 ,0.,0., 0.])
        
    def OnDelFor(self,event):
        self.gridForces1.DeleteRows(self.For_to_del, 1, updateLabels=True)
        self.FOR = np.delete(self.FOR, self.For_to_del, 0)
        for i in range(0,len(self.FOR[:,0])):
            if self.FOR[i][0] > self.For_to_del + 1:
                self.FOR[i][0] = self.FOR[i][0] - 1
#
#class ElementForce(wx.Dialog):
#    def __init__(self,FOR,PATH):
#        title = 'Element Forces'
#        
#        self.FOR = FOR
#        self.PATH = PATH
#        
#        
#        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
#
#
#        sizer = wx.BoxSizer(wx.VERTICAL)
#
#        self.gridForces1 = wx.grid.Grid(self, size=(490,150))
#        
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.FGridUpdate)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.FGridRight)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.FGridRight) 
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowF)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowF)                
#        
#        if len(self.FOR) != 0:
#            self.gridForces1.CreateGrid(len(self.FOR),5)
#        else:
#            self.gridForces1.CreateGrid(0,5)
#        
#        self.gridForces1.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
#        self.gridForces1.SetRowLabelSize(3)
#        self.gridForces1.SetColLabelValue(0, 'Element ID')
#        self.gridForces1.SetColLabelValue(1, 'Axial force')
#        self.gridForces1.SetColLabelValue(2, 'Shear force')
#        self.gridForces1.SetColLabelValue(3, 'Moment')
#        self.gridForces1.SetColLabelValue(4, 'Position')
#        for i in range(0,3):
#            self.gridForces1.SetColSize(i, 100)
##         self.gridForces1.SetColSize(1, 80)
##         self.gridForces1.SetColSize(2, 120)
#        
#        if len(self.FOR) != 0:
#            for i in range(0,len(self.FOR)):
#                    if self.FOR[i][0] != 0:
#                        self.gridForces1.SetCellValue(i,0,str(int(self.FOR[i][0])))
#                    for j in range(1,len(self.FOR[0][:])):
#                        if self.FOR[i][j] != 0:
#                            self.gridForces1.SetCellValue(i,j,str(self.FOR[i][j]))
#
#        sizer.Add(self.gridForces1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        
#        self.buttonF1 = wx.Button(self, label="Add")
#        self.Bind(wx.EVT_BUTTON, self.OnClickF, self.buttonF1)
#        
#        self.buttonDelF1 = wx.Button(self, label="Delete")
#        self.Bind(wx.EVT_BUTTON, self.ClickDelFor, self.buttonDelF1)
#
#        hbox = wx.BoxSizer(wx.HORIZONTAL)
#        hbox.Add(self.buttonF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        hbox.Add(self.buttonDelF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#
##         Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#        Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#
#        quote = wx.StaticText(self, label="The variable Position, is the position of the force relative to the length of the element: x_local / L which lies in [0,1]")
#        font = wx.Font(9, wx.DEFAULT, wx.NORMAL, wx.NORMAL)
#        quote.SetFont(font)
#        quote.Wrap(250)
#        
#        line1 = wx.StaticLine(self, -1, size=(2,-1), style=wx.LI_VERTICAL)
#        
#        sizer.Add(hbox, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        
#        hbox = wx.BoxSizer(wx.HORIZONTAL)
#        hbox.Add(Img, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        hbox.Add(line1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#        hbox.Add(quote, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        sizer.Add(hbox, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#
#        btnsizer = wx.StdDialogButtonSizer()
#        
#        if wx.Platform != "__WXMSW__":
#            btn = wx.ContextHelpButton(self)
#            btnsizer.AddButton(btn)
#        
#        btn = wx.Button(self, wx.ID_OK)
#        btn.SetDefault()
#        btnsizer.AddButton(btn)
#
#        btn = wx.Button(self, wx.ID_CANCEL)
#        btnsizer.AddButton(btn)
#        btnsizer.Realize()
#
#        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        self.SetSizer(sizer)
#        sizer.Fit(self)
#
#    def ActiveRowF(self,event):
#        self.active_rowF = event.GetRow()
#        event.Skip()
#
#    def ClickDelFor(self,event):
#        self.For_to_del = self.active_rowF
#        self.OnDelFor(0)
#
#    def FGridUpdate(self, event):
#        R = event.GetRow()
#        C = event.GetCol()
#        Value = float(self.gridForces1.GetCellValue(R, C))
#        self.FOR[R][C] = Value
#            
#    def FGridRight(self,event):
#        self.For_to_del = event.GetRow()
#        FRMenu = wx.Menu()
#        item = wx.MenuItem(FRMenu, wx.NewId(), "Delete Load")
#        FRMenu.AppendItem(item)
#        self.Bind(wx.EVT_MENU, self.OnDelFor, item)
#        self.PopupMenu(FRMenu)
#        
#    def OnClickF(self,event):
#        self.gridForces1.AppendRows(1, updateLabels = True)
#        self.FOR.append([0 ,0.,0., 0., 0.])
#        
#    def OnDelFor(self,event):
#        self.gridForces1.DeleteRows(self.For_to_del, 1, updateLabels=True)
#        self.FOR = np.delete(self.FOR, self.For_to_del, 0)
#        for i in range(0,len(self.FOR[:,0])):
#            if self.FOR[i][0] > self.For_to_del + 1:
#                self.FOR[i][0] = self.FOR[i][0] - 1
#        
#class DistributedLoad(wx.Dialog):
#    def __init__(self,FOR,PATH):
#        title = 'Distributed Loads'
#        
#        self.FOR = FOR
#        self.PATH = PATH
#        
#        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
#
#
#        sizer = wx.BoxSizer(wx.VERTICAL)
#
#        self.gridForces1 = wx.grid.Grid(self, size=(490,150))
#        
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.FGridUpdate)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.FGridRight)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.FGridRight)   
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowF)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowF)              
#        
#        if len(self.FOR) != 0:
#            self.gridForces1.CreateGrid(len(self.FOR),4)
#        else:
#            self.gridForces1.CreateGrid(0,4)
#            
#        self.gridForces1.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
#        self.gridForces1.SetRowLabelSize(3)
#        self.gridForces1.SetColLabelValue(0, 'Element ID')
#        self.gridForces1.SetColLabelValue(1, 'Axial load')
#        self.gridForces1.SetColLabelValue(2, 'Shear load')
#        self.gridForces1.SetColLabelValue(3, 'Moment')
##         self.gridForces1.SetColLabelValue(4, 'Position')
#        for i in range(0,3):
#            self.gridForces1.SetColSize(i, 100)
##         self.gridForces1.SetColSize(2, 120)
#        
#        if len(self.FOR) != 0:
#            for i in range(0,len(self.FOR)):
#                    if self.FOR[i][0] != 0:
#                        self.gridForces1.SetCellValue(i,0,str(int(self.FOR[i][0])))
#                    for j in range(1,len(self.FOR[0][:])-1):
#                        if self.FOR[i][j] != 0:
#                            self.gridForces1.SetCellValue(i,j,str(self.FOR[i][j]))
#
#        sizer.Add(self.gridForces1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        self.buttonF1 = wx.Button(self, label="Add")
#        self.Bind(wx.EVT_BUTTON, self.OnClickF, self.buttonF1)
#        
#        self.buttonDelF1 = wx.Button(self, label="Delete")
#        self.Bind(wx.EVT_BUTTON, self.ClickDelFor, self.buttonDelF1)
#
#        hbox = wx.BoxSizer(wx.HORIZONTAL)
#        hbox.Add(self.buttonF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        hbox.Add(self.buttonDelF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
##         Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#        Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#
#        sizer.Add(hbox, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        sizer.Add(Img, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#
#        btnsizer = wx.StdDialogButtonSizer()
#        
#        if wx.Platform != "__WXMSW__":
#            btn = wx.ContextHelpButton(self)
#            btnsizer.AddButton(btn)
#        
#        btn = wx.Button(self, wx.ID_OK)
#        btn.SetDefault()
#        btnsizer.AddButton(btn)
#
#        btn = wx.Button(self, wx.ID_CANCEL)
#        btnsizer.AddButton(btn)
#        btnsizer.Realize()
#
#        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        self.SetSizer(sizer)
#        sizer.Fit(self)
#
#    def ActiveRowF(self,event):
#        self.active_rowF = event.GetRow()
#        event.Skip()
#
#    def ClickDelFor(self,event):
#        self.For_to_del = self.active_rowF
#        self.OnDelFor(0)
#
#    def FGridUpdate(self, event):
#        R = event.GetRow()
#        C = event.GetCol()
#        Value = float(self.gridForces1.GetCellValue(R, C))
#        self.FOR[R][C] = Value
#            
#    def FGridRight(self,event):
#        self.For_to_del = event.GetRow()
#        FRMenu = wx.Menu()
#        item = wx.MenuItem(FRMenu, wx.NewId(), "Delete Load")
#        FRMenu.AppendItem(item)
#        self.Bind(wx.EVT_MENU, self.OnDelFor, item)
#        self.PopupMenu(FRMenu)
#        
#    def OnClickF(self,event):
#        self.gridForces1.AppendRows(1, updateLabels = True)
#        self.FOR.append([0 ,0.,0., 0., 1.])
#        
#    def OnDelFor(self,event):
#        self.gridForces1.DeleteRows(self.For_to_del, 1, updateLabels=True)
#        self.FOR = np.delete(self.FOR, self.For_to_del, 0)
#        for i in range(0,len(self.FOR[:][0])):
#            if self.FOR[i][0] > self.For_to_del + 1:
#                self.FOR[i][0] = self.FOR[i][0] - 1        
#                
#                
#class TemperatureLoad(wx.Dialog):
#    def __init__(self,FOR,PATH):
#        title = 'Temperature Loads'
#        
#        self.FOR = FOR
#        self.PATH = PATH
#        
#        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
#
#
#        sizer = wx.BoxSizer(wx.VERTICAL)
#
#        self.gridForces1 = wx.grid.Grid(self, size=(370,150))
#        
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.FGridUpdate)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.FGridRight)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.FGridRight) 
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowF)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowF)                
#        
#        if len(self.FOR) != 0:
#            self.gridForces1.CreateGrid(len(self.FOR),4)
#        else:
#            self.gridForces1.CreateGrid(0,4)
#            
#        self.gridForces1.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
#        self.gridForces1.SetRowLabelSize(3)
#        self.gridForces1.SetColLabelValue(0, 'Element ID')
#        self.gridForces1.SetColLabelValue(1, 'T_in ('u'\N{DEGREE SIGN}C)')
#        self.gridForces1.SetColLabelValue(2, 'T_out ('u'\N{DEGREE SIGN}C)')
#        self.gridForces1.SetColLabelValue(3, 'T_0 ('u'\N{DEGREE SIGN}C)')
#        for i in range(0,1):
#            self.gridForces1.SetColSize(i, 100)
##         self.gridForces1.SetColSize(2, 120)
#        
#        if len(self.FOR) != 0:
#            for i in range(0,len(self.FOR)):
#                    if self.FOR[i][0] != 0:
#                        self.gridForces1.SetCellValue(i,0,str(int(self.FOR[i][0])))
#                    for j in range(1,4):
#                        if self.FOR[i][j] != 0:
#                            self.gridForces1.SetCellValue(i,j,str(self.FOR[i][j]))
#
#        sizer.Add(self.gridForces1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        self.buttonF1 = wx.Button(self, label="Add")
#        self.Bind(wx.EVT_BUTTON, self.OnClickF, self.buttonF1)
#        
#        self.buttonDelF1 = wx.Button(self, label="Delete")
#        self.Bind(wx.EVT_BUTTON, self.ClickDelFor, self.buttonDelF1)
#
#        hbox = wx.BoxSizer(wx.HORIZONTAL)
#        hbox.Add(self.buttonF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        hbox.Add(self.buttonDelF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#
##         Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_tl.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#        Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_tl.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#
#        sizer.Add(hbox, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#
#        sizer.Add(Img, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#
#        btnsizer = wx.StdDialogButtonSizer()
#        
#        if wx.Platform != "__WXMSW__":
#            btn = wx.ContextHelpButton(self)
#            btnsizer.AddButton(btn)
#        
#        btn = wx.Button(self, wx.ID_OK)
#        btn.SetDefault()
#        btnsizer.AddButton(btn)
#
#        btn = wx.Button(self, wx.ID_CANCEL)
#        btnsizer.AddButton(btn)
#        btnsizer.Realize()
#
#        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        self.SetSizer(sizer)
#        sizer.Fit(self)
#
#    def ActiveRowF(self,event):
#        self.active_rowF = event.GetRow()
#        event.Skip()
#
#    def ClickDelFor(self,event):
#        self.For_to_del = self.active_rowF
#        self.OnDelFor(0)
#
#    def FGridUpdate(self, event):
#        R = event.GetRow()
#        C = event.GetCol()
#        Value = float(self.gridForces1.GetCellValue(R, C))
#        self.FOR[R][C] = Value
#            
#    def FGridRight(self,event):
#        self.For_to_del = event.GetRow()
#        FRMenu = wx.Menu()
#        item = wx.MenuItem(FRMenu, wx.NewId(), "Delete Load")
#        FRMenu.AppendItem(item)
#        self.Bind(wx.EVT_MENU, self.OnDelFor, item)
#        self.PopupMenu(FRMenu)
#        
#    def OnClickF(self,event):
#        self.gridForces1.AppendRows(1, updateLabels = True)
#        self.FOR = np.append(self.FOR, [[0 ,0.,0., 0.]], axis=0)
#        
#    def OnDelFor(self,event):
#        self.gridForces1.DeleteRows(self.For_to_del, 1, updateLabels=True)
#        self.FOR = np.delete(self.FOR, self.For_to_del, 0)
#        for i in range(0,len(self.FOR[:,0])):
#            if self.FOR[i][0] > self.For_to_del + 1:
#                self.FOR[i][0] = self.FOR[i][0] - 1    
#                
#            
#class SupportDisplacement(wx.Dialog):
#    def __init__(self,FOR,PATH):
#        title = 'Support Displacements'
#        
#        self.FOR = FOR
#        self.PATH = PATH
#        
#        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
#
#
#        sizer = wx.BoxSizer(wx.VERTICAL)
#
#        self.gridForces1 = wx.grid.Grid(self, size=(350,150))
#        
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.FGridUpdate)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.FGridRight)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.FGridRight)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowF)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowF)                 
#        
#        if len(self.FOR) != 0:
#            self.gridForces1.CreateGrid(len(self.FOR),4)
#        else:
#            self.gridForces1.CreateGrid(0,4)
#            
#        self.gridForces1.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
#        self.gridForces1.SetRowLabelSize(3)
#        self.gridForces1.SetColLabelValue(0, 'Node ID')
#        self.gridForces1.SetColLabelValue(1, u'\N{greek small letter delta}x')
#        self.gridForces1.SetColLabelValue(2, u'\N{greek small letter delta}y')
#        self.gridForces1.SetColLabelValue(3, u'\N{greek small letter delta}\N{greek small letter phi}')
#
#        for i in range(0,1):
#            self.gridForces1.SetColSize(i, 100)
##         self.gridForces1.SetColSize(2, 120)
#        
#        if len(self.FOR) != 0:
#            for i in range(0,len(self.FOR)):
#                    if self.FOR[i][0] != 0:
#                        self.gridForces1.SetCellValue(i,0,str(int(self.FOR[i][0])))
#                    for j in range(1,4):
#                        if self.FOR[i][j] != 0:
#                            self.gridForces1.SetCellValue(i,j,str(self.FOR[i][j]))
#
#        sizer.Add(self.gridForces1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        self.buttonF1 = wx.Button(self, label="Add")
#        self.Bind(wx.EVT_BUTTON, self.OnClickF, self.buttonF1)
#        
#        self.buttonDelF1 = wx.Button(self, label="Delete")
#        self.Bind(wx.EVT_BUTTON, self.ClickDelFor, self.buttonDelF1)
#
#        hbox = wx.BoxSizer(wx.HORIZONTAL)
#        hbox.Add(self.buttonF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        hbox.Add(self.buttonDelF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#
##         Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#        Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#
#        sizer.Add(hbox, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        sizer.Add(Img, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#
#        btnsizer = wx.StdDialogButtonSizer()
#        
#        if wx.Platform != "__WXMSW__":
#            btn = wx.ContextHelpButton(self)
#            btnsizer.AddButton(btn)
#        
#        btn = wx.Button(self, wx.ID_OK)
#        btn.SetDefault()
#        btnsizer.AddButton(btn)
#
#        btn = wx.Button(self, wx.ID_CANCEL)
#        btnsizer.AddButton(btn)
#        btnsizer.Realize()
#
#        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        self.SetSizer(sizer)
#        sizer.Fit(self)
#
#    def ActiveRowF(self,event):
#        self.active_rowM = event.GetRow()
#        event.Skip()
#
#    def ClickDelFor(self,event):
#        self.For_to_del = self.active_rowM
#        self.OnDelFor(0)
#
#    def FGridUpdate(self, event):
#        R = event.GetRow()
#        C = event.GetCol()
#        Value = float(self.gridForces1.GetCellValue(R, C))
#        self.FOR[R][C] = Value
#            
#    def FGridRight(self,event):
#        self.For_to_del = event.GetRow()
#        FRMenu = wx.Menu()
#        item = wx.MenuItem(FRMenu, wx.NewId(), "Delete Load")
#        FRMenu.AppendItem(item)
#        self.Bind(wx.EVT_MENU, self.OnDelFor, item)
#        self.PopupMenu(FRMenu)
#        
#    def OnClickF(self,event):
#        self.gridForces1.AppendRows(1, updateLabels = True)
#        self.FOR = np.append(self.FOR, [[0 ,0.,0., 0.]], axis=0)
#        
#    def OnDelFor(self,event):
#        self.gridForces1.DeleteRows(self.For_to_del, 1, updateLabels=True)
#        self.FOR = np.delete(self.FOR, self.For_to_del, 0)
#        for i in range(0,len(self.FOR[:,0])):
#            if self.FOR[i][0] > self.For_to_del + 1:
#                self.FOR[i][0] = self.FOR[i][0] - 1        
#                
#                
#class StructuralDefect(wx.Dialog):
#    def __init__(self,FOR,PATH):
#        title = 'Structural Defects'
#        
#        self.FOR = FOR
#        self.PATH = PATH
#        
#        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
#
#
#        sizer = wx.BoxSizer(wx.VERTICAL)
#
#        self.gridForces1 = wx.grid.Grid(self, size=(490,150))
#        
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.FGridUpdate)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.FGridRight)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.FGridRight)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRowF)
#        self.gridForces1.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRowF)                 
#        
#        if len(self.FOR) != 0:
#            self.gridForces1.CreateGrid(len(self.FOR[:,0]),len(self.FOR[0,:]))
#        else:
#            self.gridForces1.CreateGrid(0,5)
#            
#        self.gridForces1.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
#        self.gridForces1.SetRowLabelSize(3)
#        self.gridForces1.SetColLabelValue(0, 'Element ID')
#        self.gridForces1.SetColLabelValue(1, 'x')
#        self.gridForces1.SetColLabelValue(2, 'y')
#        self.gridForces1.SetColLabelValue(3, 'phi')
#        self.gridForces1.SetColLabelValue(4, 'Position')
#        for i in range(0,3):
#            self.gridForces1.SetColSize(i, 100)
##         self.gridForces1.SetColSize(1, 80)
##         self.gridForces1.SetColSize(2, 120)
#        
#        if len(self.FOR) != 0:
#            for i in range(0,len(self.FOR[:,0])):
#                    if self.FOR[i,0] != 0:
#                        self.gridForces1.SetCellValue(i,0,str(int(self.FOR[i,0])))
#                    for j in range(1,len(self.FOR[0,:])):
#                        if self.FOR[i,j] != 0:
#                            self.gridForces1.SetCellValue(i,j,str(self.FOR[i,j]))
#
#        sizer.Add(self.gridForces1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        self.buttonF1 = wx.Button(self, label="Add")
#        self.Bind(wx.EVT_BUTTON, self.OnClickF, self.buttonF1)
#        
#        self.buttonDelF1 = wx.Button(self, label="Delete")
#        self.Bind(wx.EVT_BUTTON, self.ClickDelFor, self.buttonDelF1)
#
#        hbox = wx.BoxSizer(wx.HORIZONTAL)
#        hbox.Add(self.buttonF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#        hbox.Add(self.buttonDelF1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#
#        sizer.Add(hbox, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#
#        btnsizer = wx.StdDialogButtonSizer()
#        
#        if wx.Platform != "__WXMSW__":
#            btn = wx.ContextHelpButton(self)
#            btnsizer.AddButton(btn)
#        
#        btn = wx.Button(self, wx.ID_OK)
#        btn.SetDefault()
#        btnsizer.AddButton(btn)
#
#        btn = wx.Button(self, wx.ID_CANCEL)
#        btnsizer.AddButton(btn)
#        btnsizer.Realize()
#
#        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#
#        self.SetSizer(sizer)
#        sizer.Fit(self)
#
#    def ActiveRowF(self,event):
#        self.active_rowM = event.GetRow()
#        event.Skip()
#
#    def ClickDelFor(self,event):
#        self.For_to_del = self.active_rowM
#        self.OnDelFor(0)
#
#    def FGridUpdate(self, event):
#        R = event.GetRow()
#        C = event.GetCol()
#        Value = float(self.gridForces1.GetCellValue(R, C))
#        self.FOR[R][C] = Value
#            
#    def FGridRight(self,event):
#        self.For_to_del = event.GetRow()
#        FRMenu = wx.Menu()
#        item = wx.MenuItem(FRMenu, wx.NewId(), "Delete Defect")
#        FRMenu.AppendItem(item)
#        self.Bind(wx.EVT_MENU, self.OnDelFor, item)
#        self.PopupMenu(FRMenu)
#        
#    def OnClickF(self,event):
#        self.gridForces1.AppendRows(1, updateLabels = True)
#        self.FOR = np.append(self.FOR, [[0 ,0.,0., 0., 0.]], axis=0)
#        
#    def OnDelFor(self,event):
#        self.gridForces1.DeleteRows(self.For_to_del, 1, updateLabels=True)
#        self.FOR = np.delete(self.FOR, self.For_to_del, 0)
#        for i in range(0,len(self.FOR[:,0])):
#            if self.FOR[i][0] > self.For_to_del + 1:
#                self.FOR[i][0] = self.FOR[i][0] - 1 
#

               
                
class ViewOptions(wx.Dialog):
    def __init__(self,sF,lF,lG):
        title = 'View Options'
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)

        sizer = wx.BoxSizer(wx.VERTICAL)
        self.gbs = wx.GridBagSizer(5, 5)

        hbox1 = wx.BoxSizer(wx.HORIZONTAL)
        self.quoteN = wx.StaticText(self, label="   Nodes:              ")
        self.labelN = wx.CheckBox(self, -1, "Labels", style=wx.ALIGN_RIGHT)
        self.labelN.SetValue(lG[0])
        self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.labelN)
        
        hbox1.Add(self.quoteN, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        hbox1.Add(self.labelN, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        sizer.Add(hbox1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        
        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        
        hbox2 = wx.BoxSizer(wx.HORIZONTAL)
        self.quoteE = wx.StaticText(self, label="   Elements:          ")
        self.labelE = wx.CheckBox(self, -1, "Labels", style=wx.ALIGN_RIGHT)
        self.labelE.SetValue(lG[1])
        self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.labelE)
        
        
        hbox2.Add(self.quoteE, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        hbox2.Add(self.labelE, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        sizer.Add(hbox2, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        

        self.quoteF1 = wx.StaticText(self, label="   Nodal forces: ")
#         self.quoteF2 = wx.StaticText(self, label="   Element forces: ")
#         self.quoteF3 = wx.StaticText(self, label="   Distributed loads: ")
#         self.quoteF4 = wx.StaticText(self, label="   Temperature loads: ")
#         self.quoteF5 = wx.StaticText(self, label="   Support displacements: ")
#         self.quoteF6 = wx.StaticText(self, label="   Structural defects: ")    
          
        self.showF1 = wx.CheckBox(self, -1, "Show", style=wx.ALIGN_RIGHT)
        self.showF1.SetValue(sF[0])
        self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.showF1) 
          
#         self.showF2 = wx.CheckBox(self, -1, "Show", style=wx.ALIGN_RIGHT)
#         self.showF2.SetValue(sF[1])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.showF2) 
#           
#         self.showF3 = wx.CheckBox(self, -1, "Show", style=wx.ALIGN_RIGHT)
#         self.showF3.SetValue(sF[2])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.showF3) 
#           
#         self.showF4 = wx.CheckBox(self, -1, "Show", style=wx.ALIGN_RIGHT)
#         self.showF4.SetValue(sF[3])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.showF4) 
#           
#         self.showF5 = wx.CheckBox(self, -1, "Show", style=wx.ALIGN_RIGHT)
#         self.showF5.SetValue(sF[4])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.showF5) 
#           
#         self.showF6 = wx.CheckBox(self, -1, "Show", style=wx.ALIGN_RIGHT)
#         self.showF6.SetValue(sF[5])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.showF6)   
        
        self.labelF1 = wx.CheckBox(self, -1, "Labels", style=wx.ALIGN_RIGHT)
        self.labelF1.SetValue(lF[0])
        self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.labelF1)    
        
#         self.labelF2 = wx.CheckBox(self, -1, "Labels", style=wx.ALIGN_RIGHT)
#         self.labelF2.SetValue(lF[1])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.labelF2)
#         
#         self.labelF3 = wx.CheckBox(self, -1, "Labels", style=wx.ALIGN_RIGHT)
#         self.labelF3.SetValue(lF[2])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.labelF3)
#         
#         self.labelF4 = wx.CheckBox(self, -1, "Labels", style=wx.ALIGN_RIGHT)
#         self.labelF4.SetValue(lF[3])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.labelF4)                        
# 
#         self.labelF5 = wx.CheckBox(self, -1, "Labels", style=wx.ALIGN_RIGHT)
#         self.labelF5.SetValue(lF[4])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.labelF5)
#         
#         self.labelF6 = wx.CheckBox(self, -1, "Labels", style=wx.ALIGN_RIGHT)
#         self.labelF6.SetValue(lF[5])
#         self.Bind(wx.EVT_CHECKBOX, self.on_labelF, self.labelF6)        

        self.gbs.Add( self.quoteF1,   (0,0) )
#         self.gbs.Add( self.quoteF2,   (1,0) )
#         self.gbs.Add( self.quoteF3,   (2,0) )
#         self.gbs.Add( self.quoteF4,   (3,0) )
#         self.gbs.Add( self.quoteF5,   (4,0) )
#         self.gbs.Add( self.quoteF6,   (5,0) )

        self.gbs.Add( self.showF1,   (0,1) )
#         self.gbs.Add( self.showF2,   (1,1) )
#         self.gbs.Add( self.showF3,   (2,1) )
#         self.gbs.Add( self.showF4,   (3,1) )
#         self.gbs.Add( self.showF5,   (4,1) )
#         self.gbs.Add( self.showF6,   (5,1) )

        self.gbs.Add( self.labelF1,   (0,2) )
#         self.gbs.Add( self.labelF2,   (1,2) )
#         self.gbs.Add( self.labelF3,   (2,2) )
#         self.gbs.Add( self.labelF4,   (3,2) )
#         self.gbs.Add( self.labelF5,   (4,2) )
#         self.gbs.Add( self.labelF6,   (5,2) )

        sizer.Add(self.gbs, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        
        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def on_labelF(self,event):
        pass

    def FGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        Value = float(self.gridForces1.GetCellValue(R, C))
        self.FOR[R][C] = Value
            
    def FGridRight(self,event):
        self.For_to_del = event.GetRow()
        FRMenu = wx.Menu()
        item = wx.MenuItem(FRMenu, wx.NewId(), "Delete Force")
        FRMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelFor, item)
        self.PopupMenu(FRMenu)
        
    def OnClickF(self,event):
        self.gridForces1.AppendRows(1, updateLabels = True)
        self.FOR = np.append(self.FOR, [[0 ,0.,0., 0.]], axis=0)
        
    def OnDelFor(self,event):
        self.gridForces1.DeleteRows(self.For_to_del, 1, updateLabels=True)
        self.FOR = np.delete(self.FOR, self.For_to_del, 0)
        for i in range(0,len(self.FOR[:,0])):
            if self.FOR[i][0] > self.For_to_del + 1:
                self.FOR[i][0] = self.FOR[i][0] - 1
                
                
class Materials(wx.Dialog):
    def __init__(self,MAT,PATH):
        title = 'Materials'
        
        self.MAT = MAT
        self.PATH = PATH
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)


        sizer = wx.BoxSizer(wx.VERTICAL)

        self.gridMaterials = wx.grid.Grid(self, size=(320,150))
        
        self.gridMaterials.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.FGridUpdate)
        self.gridMaterials.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRow)
        self.gridMaterials.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRow)
        self.gridMaterials.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.FGridRight)
        self.gridMaterials.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.FGridRight)           
        
        if len(self.MAT) != 0:
            self.gridMaterials.CreateGrid(len(self.MAT),2)
        else:
            self.gridMaterials.CreateGrid(0,2)
            
        self.gridMaterials.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
        self.gridMaterials.SetRowLabelSize(100)
        self.gridMaterials.SetColLabelValue(0, 'Density')
        self.gridMaterials.SetColLabelValue(1, 'E')
#         self.gridMaterials.SetColLabelValue(2, 'a')
        
        corn_M = self.gridMaterials.GetGridCornerLabelWindow()
        M_ID = wx.StaticText(corn_M, label="Material ID",pos=(9,8))
        font = wx.Font(9, wx.DEFAULT, wx.NORMAL, wx.BOLD)
        M_ID.SetFont(font)        
        
        for i in range(0,2):
            self.gridMaterials.SetColSize(i, 100)
        
        if len(self.MAT) != 0:
            for i in range(0,len(self.MAT)):
                if self.MAT[i][0] != 0:
                    self.gridMaterials.SetCellValue(i,0,str(self.MAT[i][1]))
                    self.gridMaterials.SetCellValue(i,1,str(self.MAT[i][2]))
#                     self.gridMaterials.SetCellValue(i,2,str(self.MAT[i][3]))
                    

        sizer.Add(self.gridMaterials, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.buttonM1 = wx.Button(self, label="Add")
        self.Bind(wx.EVT_BUTTON, self.OnClickF, self.buttonM1)

        self.buttonM2 = wx.Button(self, label="Delete")
        self.Bind(wx.EVT_BUTTON, self.OnClickDel, self.buttonM2)

        hbox = wx.BoxSizer(wx.HORIZONTAL)
        hbox.Add(self.buttonM1, 0, border=3, flag=wx.ALIGN_RIGHT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        hbox.Add(self.buttonM2, 0, border=3, flag=wx.ALIGN_RIGHT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)


        sizer.Add(hbox, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)



        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def ActiveRow(self,event):
        self.active_row = event.GetRow()
        event.Skip()

    def FGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        Value = float(self.gridMaterials.GetCellValue(R, C))
        self.MAT[R][C+1] = Value
            
    def FGridRight(self,event):
        self.Mat_to_del = event.GetRow()
        FRMenu = wx.Menu()
        item = wx.MenuItem(FRMenu, wx.NewId(), "Delete Material")
        FRMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelMat, item)
        self.PopupMenu(FRMenu)
        
    def OnClickF(self,event):
        self.gridMaterials.AppendRows(1, updateLabels = True)
        R = len(self.MAT)
        print(self.MAT)
        self.MAT.append([R+1, 0. ,0.,0.])

    def OnClickDel(self,event):
        self.Mat_to_del = self.active_row
        self.OnDelMat(0)
        
    def OnDelMat(self,event):
        self.gridMaterials.DeleteRows(self.Mat_to_del, 1, updateLabels=True)
        del self.MAT[self.Mat_to_del]
        for i in range(0,len(self.MAT)):
            if self.MAT[i][0] > self.Mat_to_del + 1:
                self.MAT[i][0] = self.MAT[i][0] - 1 
                
                
class Sections(wx.Dialog):
    def __init__(self,SEC,PATH):
        title = 'Sections'
        
        self.SEC = SEC
        self.PATH = PATH
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)


        sizer = wx.BoxSizer(wx.VERTICAL)

        self.gridSections = wx.grid.Grid(self, size=(320,150))
        
        self.gridSections.Bind(wx.grid.EVT_GRID_CELL_CHANGED, self.FGridUpdate)
        self.gridSections.Bind(wx.grid.EVT_GRID_CELL_LEFT_CLICK, self.ActiveRow)
        self.gridSections.Bind(wx.grid.EVT_GRID_LABEL_LEFT_CLICK, self.ActiveRow)
        self.gridSections.Bind(wx.grid.EVT_GRID_CELL_RIGHT_CLICK, self.FGridRight)
        self.gridSections.Bind(wx.grid.EVT_GRID_LABEL_RIGHT_CLICK, self.FGridRight)           
        
        if len(self.SEC) != 0:
            self.gridSections.CreateGrid(len(self.SEC),2)
        else:
            self.gridSections.CreateGrid(0,2)
            
        self.gridSections.SetDefaultCellAlignment(wx.ALIGN_CENTRE, wx.ALIGN_CENTRE )
        self.gridSections.SetRowLabelSize(100)
        self.gridSections.SetColLabelValue(0, 'A')
#         self.gridSections.SetColLabelValue(1, 'I')
#         self.gridSections.SetColLabelValue(2, 'h')
        self.gridSections.SetColLabelValue(1, 'Material ID')
        
        corn_M = self.gridSections.GetGridCornerLabelWindow()
        M_ID = wx.StaticText(corn_M, label="Section ID",pos=(9,8))
        font = wx.Font(9, wx.DEFAULT, wx.NORMAL, wx.BOLD)
        M_ID.SetFont(font)        
        
        for i in range(0,2):
            self.gridSections.SetColSize(i, 100)
        
        if len(self.SEC) != 0:
            for i in range(0,len(self.SEC)):
                if self.SEC[i][0] != 0:
                    self.gridSections.SetCellValue(i,0,str(self.SEC[i][1]))
#                     self.gridSections.SetCellValue(i,1,str(self.SEC[i][2]))
#                     self.gridSections.SetCellValue(i,2,str(self.SEC[i][3]))
                    self.gridSections.SetCellValue(i,1,str(self.SEC[i][2]))
                    

        sizer.Add(self.gridSections, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.buttonM1 = wx.Button(self, label="Add")
        self.Bind(wx.EVT_BUTTON, self.OnClickF, self.buttonM1)

        self.buttonM2 = wx.Button(self, label="Delete")
        self.Bind(wx.EVT_BUTTON, self.OnClickDel, self.buttonM2)

        hbox = wx.BoxSizer(wx.HORIZONTAL)
        hbox.Add(self.buttonM1, 0, border=3, flag=wx.ALIGN_RIGHT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)
        hbox.Add(self.buttonM2, 0, border=3, flag=wx.ALIGN_RIGHT | wx.ALL | wx.ALIGN_CENTER_VERTICAL)


        sizer.Add(hbox, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)



        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def ActiveRow(self,event):
        self.active_row = event.GetRow()
        event.Skip()

    def FGridUpdate(self, event):
        R = event.GetRow()
        C = event.GetCol()
        Value = float(self.gridSections.GetCellValue(R, C))
        self.SEC[R][C+1] = Value
            
    def FGridRight(self,event):
        self.Sec_to_del = event.GetRow()
        FRMenu = wx.Menu()
        item = wx.MenuItem(FRMenu, wx.NewId(), "Delete Section")
        FRMenu.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.OnDelSec, item)
        self.PopupMenu(FRMenu)
        
    def OnClickF(self,event):
        self.gridSections.AppendRows(1, updateLabels = True)
        R = len(self.SEC)
        self.SEC.append([R+1, 0. ,0.,0., 0.])

    def OnClickDel(self,event):
        self.Sec_to_del = self.active_row
        self.OnDelSec(0)
        
    def OnDelSec(self,event):
        self.gridSections.DeleteRows(self.Sec_to_del, 1, updateLabels=True)
        del self.SEC[self.Sec_to_del]
        for i in range(0,len(self.SEC)):
            if self.SEC[i][0] > self.Sec_to_del + 1:
                self.SEC[i][0] = self.SEC[i][0] - 1 
                
class Support(wx.Dialog):
    def __init__(self,dat):
        title = 'Support'
        
        self.dat = dat
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
        
        self.triple = [self.dat[1],self.dat[2]]
        self.sup_angle = self.dat[3]
        sizer = wx.BoxSizer(wx.VERTICAL)
        Gsizer = wx.GridBagSizer(5,5)

        Gsizer.Add(wx.StaticText(self,-1,'X Translation:   '),(0,0))
        Gsizer.Add(wx.StaticText(self,-1,'Y Translation:   '),(1,0))
#         Gsizer.Add(wx.StaticText(self,-1,'Rotation:   '),(2,0))
        
        Gsizer.Add(wx.StaticText(self,-1,'Angle (degrees):   '),(3,0))
        
        sampleList = ['0','1']
        self.cb1 = wx.ComboBox(self, value=str(self.dat[1]), choices = sampleList,style = wx.CB_READONLY)
        self.cb2 = wx.ComboBox(self, value=str(self.dat[2]), choices = sampleList,style = wx.CB_READONLY)
#         self.cb3 = wx.ComboBox(self, value=str(self.dat[3]), choices = sampleList,style = wx.CB_READONLY)

        self.tc4 = wx.TextCtrl(self, -1, str(self.dat[3]), size=(50, -1))

        self.Bind(wx.EVT_COMBOBOX, self.EvtComboBox, self.cb1)
        self.Bind(wx.EVT_COMBOBOX, self.EvtComboBox, self.cb2)
#         self.Bind(wx.EVT_COMBOBOX, self.EvtComboBox, self.cb3)

        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc4)


        Gsizer.Add(self.cb1, (0,4))
        Gsizer.Add(self.cb2, (1,4))
#         Gsizer.Add(self.cb3, (2,4))
        
        Gsizer.Add(self.tc4, (3,4))

        sizer.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def EvtText(self, event):
        self.sup_angle = float(event.GetString())
        event.Skip()

    def EvtComboBox(self, event):
#         self.triple = [self.cb1.GetSelection(),self.cb2.GetSelection(),self.cb3.GetSelection()]
        self.triple = [self.cb1.GetSelection(),self.cb2.GetSelection()]
        event.Skip()
        

class EditElement(wx.Dialog):
    def __init__(self,dat):
        title = 'Edit Element'
        
        self.dat = dat
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        Gsizer = wx.GridBagSizer(5,5)

        if len(self.dat) == 1:
            self.dat_out = [0,0,0,0,0]
            
            Gsizer.Add(wx.StaticText(self,-1,'X'),(0,1))
            Gsizer.Add(wx.StaticText(self,-1,'Y'),(0,2))
            Gsizer.Add(wx.StaticText(self,-1,'Node 1 :   '),(1,0))
            Gsizer.Add(wx.StaticText(self,-1,'Node 2 :   '),(2,0))
    
            Gsizer.Add(wx.StaticText(self,-1,'Section ID:   '),(4,0))
    
            self.tc1 = wx.TextCtrl(self, -1, str(self.dat[0][0]), size=(50, -1))
            self.tc2 = wx.TextCtrl(self, -1, str(self.dat[0][1]), size=(50, -1))
            self.tc3 = wx.TextCtrl(self, -1, str(self.dat[0][2]), size=(50, -1))
            self.tc4 = wx.TextCtrl(self, -1, str(self.dat[0][3]), size=(50, -1))
            
            self.tc5 = wx.TextCtrl(self, -1, str(self.dat[0][4]), size=(50, -1))
    
            self.Bind(wx.EVT_TEXT, self.EvtText, self.tc1)
            self.Bind(wx.EVT_TEXT, self.EvtText, self.tc2)
            self.Bind(wx.EVT_TEXT, self.EvtText, self.tc3)
            self.Bind(wx.EVT_TEXT, self.EvtText, self.tc4)
            
            self.Bind(wx.EVT_TEXT, self.EvtText, self.tc5)
    
            Gsizer.Add(self.tc1, (1,1))
            Gsizer.Add(self.tc2, (1,2))
            Gsizer.Add(self.tc3, (2,1))
            Gsizer.Add(self.tc4, (2,2))
            
            Gsizer.Add(self.tc5, (4,1))
        
        else:
            Gsizer.Add(wx.StaticText(self,-1,'Section ID:   '),(0,0))
            self.tc5 = wx.TextCtrl(self, -1, str(self.dat[0][4]), size=(50, -1))
            self.Bind(wx.EVT_TEXT, self.EvtText, self.tc5)
            Gsizer.Add(self.tc5, (0,1))
            
            
        sizer.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def EvtText(self, event):
        if len(self.dat) == 1:
            self.dat_out[0] = float(self.tc1.GetValue())
            self.dat_out[1] = float(self.tc2.GetValue())
            self.dat_out[2] = float(self.tc3.GetValue())
            self.dat_out[3] = float(self.tc4.GetValue())
            
            self.dat_out[4] = float(self.tc5.GetValue())
        else:
            self.dat_out = float(self.tc5.GetValue())

        event.Skip()
# 
#     def EvtComboBox(self, event):
#         self.triple = [self.cb1.GetSelection(),self.cb2.GetSelection(),self.cb3.GetSelection()]
#         event.Skip()
        
class F1(wx.Dialog):
    def __init__(self,dat, PATH):
        title = 'Loads on Node'
        
        self.dat = dat
        self.PATH = PATH
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
        
        self.dat_out = [self.dat[1],self.dat[2],self.dat[3]]
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        Gsizer = wx.GridBagSizer(5,5)
            
        Gsizer.Add(wx.StaticText(self,-1,'X Force: '),(0,0))
        Gsizer.Add(wx.StaticText(self,-1,'Y Force: '),(1,0))
#         Gsizer.Add(wx.StaticText(self,-1,'Moment:  '),(2,0))

        self.tc1 = wx.TextCtrl(self, -1, str(self.dat[1]), size=(50, -1))
        self.tc2 = wx.TextCtrl(self, -1, str(self.dat[2]), size=(50, -1))
#         self.tc3 = wx.TextCtrl(self, -1, str(self.dat[3]), size=(50, -1))

        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc1)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc2)
#         self.Bind(wx.EVT_TEXT, self.EvtText, self.tc3)


        Gsizer.Add(self.tc1, (0,4))
        Gsizer.Add(self.tc2, (1,4))
#         Gsizer.Add(self.tc3, (2,4))
            
        sizer.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

#         Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
        Img = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())

        sizer.Add(Img, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def EvtText(self, event):
        self.dat_out[0] = float(self.tc1.GetValue())
        self.dat_out[1] = float(self.tc2.GetValue())
#         self.dat_out[2] = float(self.tc3.GetValue())

        event.Skip()
        
        
class F2(wx.Dialog):
    def __init__(self,dat, PATH):
        title = 'Loads on Element'
        
        self.dat = dat
        self.PATH = PATH
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
        
        self.dat_out = [self.dat[1],self.dat[2],self.dat[3],self.dat[4]]
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        Gsizer = wx.GridBagSizer(5,5)
            
        Gsizer.Add(wx.StaticText(self,-1,'    X Force: '),(0,0))
        Gsizer.Add(wx.StaticText(self,-1,'    Y Force: '),(1,0))
        Gsizer.Add(wx.StaticText(self,-1,'    Moment:  '),(2,0))
        Gsizer.Add(wx.StaticText(self,-1,'    Relative Position:  '),(3,0))

        self.tc1 = wx.TextCtrl(self, -1, str(self.dat[1]), size=(50, -1))
        self.tc2 = wx.TextCtrl(self, -1, str(self.dat[2]), size=(50, -1))
        self.tc3 = wx.TextCtrl(self, -1, str(self.dat[3]), size=(50, -1))
        self.tc4 = wx.TextCtrl(self, -1, str(self.dat[4]), size=(50, -1))

        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc1)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc2)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc3)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc4)


        Gsizer.Add(self.tc1, (0,4))
        Gsizer.Add(self.tc2, (1,4))
        Gsizer.Add(self.tc3, (2,4))
        Gsizer.Add(self.tc4, (3,4))
            
        sizer.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        sizer2 = wx.BoxSizer(wx.HORIZONTAL)
        
        sizer3 = wx.BoxSizer(wx.VERTICAL)
        sizer4 = wx.BoxSizer(wx.VERTICAL)
        
        self.Local = True
        self.radio1 = wx.RadioButton(self, -1, " Local CS ", style = wx.RB_GROUP )
        self.radio2 = wx.RadioButton(self, -1, " Global CS " )
        self.Bind(wx.EVT_RADIOBUTTON, self.OnRadioSelect, self.radio1 )
        self.Bind(wx.EVT_RADIOBUTTON, self.OnRadioSelect, self.radio2 )

#         Img1 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#         Img2 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_ef_glob.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
        Img1 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
        Img2 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_ef_glob.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())

        sizer3.Add(self.radio1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        sizer3.Add(Img1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        
        sizer4.Add(self.radio2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        sizer4.Add(Img2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        
        sizer2.Add(sizer3, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        line = wx.StaticLine(self, -1, size=(-1,20), style=wx.LI_VERTICAL)
        sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        sizer2.Add(sizer4, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        sizer.Add(sizer2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnRadioSelect(self,event):
        radio_selected = event.GetEventObject()
        if radio_selected is self.radio1:
            self.Local = True
        elif radio_selected is self.radio2:
            self.Local = False

    def EvtText(self, event):
        self.dat_out[0] = float(self.tc1.GetValue())
        self.dat_out[1] = float(self.tc2.GetValue())
        self.dat_out[2] = float(self.tc3.GetValue())
        self.dat_out[3] = float(self.tc4.GetValue())

        event.Skip()
        
class F3(wx.Dialog):
    def __init__(self,dat, PATH):
        title = 'Distributed Load'
        
        self.dat = dat
        self.PATH = PATH
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
        
        self.dat_out = [self.dat[1],self.dat[2],self.dat[3]]
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        Gsizer = wx.GridBagSizer(5,5)
            
        Gsizer.Add(wx.StaticText(self,-1,'    X Load: '),(0,0))
        Gsizer.Add(wx.StaticText(self,-1,'    Y Load: '),(1,0))
        Gsizer.Add(wx.StaticText(self,-1,'    Moment:  '),(2,0))

        self.tc1 = wx.TextCtrl(self, -1, str(self.dat[1]), size=(50, -1))
        self.tc2 = wx.TextCtrl(self, -1, str(self.dat[2]), size=(50, -1))
        self.tc3 = wx.TextCtrl(self, -1, str(self.dat[3]), size=(50, -1))

        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc1)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc2)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc3)

        Gsizer.Add(self.tc1, (0,4))
        Gsizer.Add(self.tc2, (1,4))
        Gsizer.Add(self.tc3, (2,4))
            
        sizer.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        sizer2 = wx.BoxSizer(wx.HORIZONTAL)
        
        sizer3 = wx.BoxSizer(wx.VERTICAL)
        sizer4 = wx.BoxSizer(wx.VERTICAL)
        
        self.Local = True
        self.radio1 = wx.RadioButton(self, -1, " Local CS ", style = wx.RB_GROUP )
        self.radio2 = wx.RadioButton(self, -1, " Global CS " )
        self.Bind(wx.EVT_RADIOBUTTON, self.OnRadioSelect, self.radio1 )
        self.Bind(wx.EVT_RADIOBUTTON, self.OnRadioSelect, self.radio2 )

#         Img1 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#         Img2 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_ef_glob.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
        Img1 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_ef.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
        Img2 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_ef_glob.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())

        sizer3.Add(self.radio1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        sizer3.Add(Img1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        
        sizer4.Add(self.radio2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        sizer4.Add(Img2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        
        sizer2.Add(sizer3, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        line = wx.StaticLine(self, -1, size=(-1,20), style=wx.LI_VERTICAL)
        sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        sizer2.Add(sizer4, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        sizer.Add(sizer2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnRadioSelect(self,event):
        radio_selected = event.GetEventObject()
        if radio_selected is self.radio1:
            self.Local = True
        elif radio_selected is self.radio2:
            self.Local = False

    def EvtText(self, event):
        self.dat_out[0] = float(self.tc1.GetValue())
        self.dat_out[1] = float(self.tc2.GetValue())
        self.dat_out[2] = float(self.tc3.GetValue())

        event.Skip()
        
class F4(wx.Dialog):
    def __init__(self,dat, PATH):
        title = 'Temperature Load'
        
        self.dat = dat
        self.PATH = PATH
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
        
        self.dat_out = [self.dat[1],self.dat[2],self.dat[3]]
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        Gsizer = wx.GridBagSizer(5,5)
            
        Gsizer.Add(wx.StaticText(self,-1,'    T in: '),(0,0))
        Gsizer.Add(wx.StaticText(self,-1,'    T out: '),(1,0))
        Gsizer.Add(wx.StaticText(self,-1,'    T 0:  '),(2,0))

        self.tc1 = wx.TextCtrl(self, -1, str(self.dat[1]), size=(50, -1))
        self.tc2 = wx.TextCtrl(self, -1, str(self.dat[2]), size=(50, -1))
        self.tc3 = wx.TextCtrl(self, -1, str(self.dat[3]), size=(50, -1))

        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc1)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc2)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc3)

        Gsizer.Add(self.tc1, (0,4))
        Gsizer.Add(self.tc2, (1,4))
        Gsizer.Add(self.tc3, (2,4))
            
        sizer.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

#         sizer2 = wx.BoxSizer(wx.HORIZONTAL)
#         
#         sizer3 = wx.BoxSizer(wx.VERTICAL)
#         sizer4 = wx.BoxSizer(wx.VERTICAL)
#         
#         self.Local = True
#         self.radio1 = wx.RadioButton(self, -1, " Local CS ", style = wx.RB_GROUP )
#         self.radio2 = wx.RadioButton(self, -1, " Global CS " )
#         self.Bind(wx.EVT_RADIOBUTTON, self.OnRadioSelect, self.radio1 )
#         self.Bind(wx.EVT_RADIOBUTTON, self.OnRadioSelect, self.radio2 )

#         Img1 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_tl.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
        Img1 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_tl.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#         Img2 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_ef_glob.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())

#         sizer3.Add(self.radio1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
#         sizer3.Add(Img1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
#         
#         sizer4.Add(self.radio2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
#         sizer4.Add(Img2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
#         
#         sizer2.Add(sizer3, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#         line = wx.StaticLine(self, -1, size=(-1,20), style=wx.LI_VERTICAL)
#         sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#         sizer2.Add(sizer4, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        sizer.Add(Img1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnRadioSelect(self,event):
        radio_selected = event.GetEventObject()
        if radio_selected is self.radio1:
            self.Local = True
        elif radio_selected is self.radio2:
            self.Local = False

    def EvtText(self, event):
        self.dat_out[0] = float(self.tc1.GetValue())
        self.dat_out[1] = float(self.tc2.GetValue())
        self.dat_out[2] = float(self.tc3.GetValue())

        event.Skip()
        
        
class F5(wx.Dialog):
    def __init__(self,dat, PATH):
        title = 'Temperature Load'
        
        self.dat = dat
        self.PATH = PATH
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
        
        self.dat_out = [self.dat[1],self.dat[2],self.dat[3]]
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        Gsizer = wx.GridBagSizer(5,5)
            
        Gsizer.Add(wx.StaticText(self,-1,'    Delta X: '),(0,0))
        Gsizer.Add(wx.StaticText(self,-1,'    Delta Y: '),(1,0))
        Gsizer.Add(wx.StaticText(self,-1,'    Delta phi:  '),(2,0))

        self.tc1 = wx.TextCtrl(self, -1, str(self.dat[1]), size=(50, -1))
        self.tc2 = wx.TextCtrl(self, -1, str(self.dat[2]), size=(50, -1))
        self.tc3 = wx.TextCtrl(self, -1, str(self.dat[3]), size=(50, -1))

        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc1)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc2)
        self.Bind(wx.EVT_TEXT, self.EvtText, self.tc3)

        Gsizer.Add(self.tc1, (0,4))
        Gsizer.Add(self.tc2, (1,4))
        Gsizer.Add(self.tc3, (2,4))
            
        sizer.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

#         sizer2 = wx.BoxSizer(wx.HORIZONTAL)
#         
#         sizer3 = wx.BoxSizer(wx.VERTICAL)
#         sizer4 = wx.BoxSizer(wx.VERTICAL)
#         
#         self.Local = True
#         self.radio1 = wx.RadioButton(self, -1, " Local CS ", style = wx.RB_GROUP )
#         self.radio2 = wx.RadioButton(self, -1, " Global CS " )
#         self.Bind(wx.EVT_RADIOBUTTON, self.OnRadioSelect, self.radio1 )
#         self.Bind(wx.EVT_RADIOBUTTON, self.OnRadioSelect, self.radio2 )

#         Img1 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
        Img1 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '/icons/pos_nf.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())
#         Img2 = wx.StaticBitmap(self, bitmap=wx.Image(self.PATH + '\icons\\pos_ef_glob.png',wx.BITMAP_TYPE_PNG).ConvertToBitmap())

#         sizer3.Add(self.radio1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
#         sizer3.Add(Img1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
#         
#         sizer4.Add(self.radio2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
#         sizer4.Add(Img2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
#         
#         sizer2.Add(sizer3, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#         line = wx.StaticLine(self, -1, size=(-1,20), style=wx.LI_VERTICAL)
#         sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
#         sizer2.Add(sizer4, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        sizer.Add(Img1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnRadioSelect(self,event):
        radio_selected = event.GetEventObject()
        if radio_selected is self.radio1:
            self.Local = True
        elif radio_selected is self.radio2:
            self.Local = False

    def EvtText(self, event):
        self.dat_out[0] = float(self.tc1.GetValue())
        self.dat_out[1] = float(self.tc2.GetValue())
        self.dat_out[2] = float(self.tc3.GetValue())

        event.Skip()
        
class Analysis_Options(wx.Dialog):
    def __init__(self,dat,n_modes):
        title = 'Analysis Options'
        
        self.dat = dat
        self.n_modes = n_modes
        
        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)

        self.nb = wx.Notebook(self)
        
        self.StaticTab = wx.Window(self.nb)
        self.ModalTab = wx.Window(self.nb)
        self.StaticTab.SetBackgroundColour("white")
        self.ModalTab.SetBackgroundColour("white")
        
        self.nb.AddPage(self.StaticTab, "Static Analysis")
        self.nb.AddPage(self.ModalTab, "Modal Analysis")
        
        self.dat_out = self.dat
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer1 = wx.BoxSizer(wx.VERTICAL)
        
        sizer1.AddSpacer(10)
        sizer1.Add(wx.StaticText(self.StaticTab,-1,'    Solution of the linear system using: '))
        sizer1.AddSpacer(10)
        
        self.group1_ctrls = []
        radio1 = wx.RadioButton( self.StaticTab, -1, " Gaussian elimination  ", style = wx.RB_GROUP )
        radio2 = wx.RadioButton( self.StaticTab, -1, " LU decomposition  " )
        
        self.group1_ctrls.append((radio1))
        self.group1_ctrls.append((radio2))
        
        if self.dat[0] == 1:
            radio1.SetValue(True)
        elif self.dat[0] == 2:
            radio2.SetValue(True)
        
        Gsizer = wx.GridBagSizer(5,5)
        Gsizer.Add(radio1,(0,0))
        Gsizer.Add(radio2,(0,1))
        sizer1.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self.StaticTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer1.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        sizer1.AddSpacer(10)           
        
        hbox = wx.BoxSizer(wx.HORIZONTAL)
        hbox.Add(wx.StaticText(self.StaticTab,-1,' Calculation of diagrams in '))
        acc = wx.TextCtrl( self.StaticTab, -1, str(int(self.dat[11])), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtAcc, acc)
        hbox.Add(acc)
        hbox.Add(wx.StaticText(self.StaticTab,-1,' points per element.'))
        
        sizer1.Add(hbox, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        
        sizer2 = wx.BoxSizer(wx.VERTICAL)
        
        sizer2.AddSpacer(10)
        sizer2.Add(wx.StaticText(self.ModalTab,-1,'    Solution method for the eigenvalue problem: '))
        sizer2.AddSpacer(10)
        
        self.group2_ctrls = []
        radio10 = wx.RadioButton( self.ModalTab, -1, " Jacobi  ", style = wx.RB_GROUP )
        radio20 = wx.RadioButton( self.ModalTab, -1, " Subspace iteration  " )
        radio30 = wx.RadioButton( self.ModalTab, -1, " HQRI  " )

        self.group2_ctrls.append((radio10))
        self.group2_ctrls.append((radio20))
        self.group2_ctrls.append((radio30))

        if self.dat[1] == 1:
            radio10.SetValue(True)
        elif self.dat[1] == 2:
            radio20.SetValue(True)
        elif self.dat[1] == 3:
            radio30.SetValue(True)
        
        Gsizer2 = wx.GridBagSizer(5,5)
        Gsizer2.Add(radio10,(0,0))
        Gsizer2.Add(radio20,(0,1))
        Gsizer2.Add(radio30,(1,0))
        sizer2.Add(Gsizer2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        
        samplelist = ['all']
        for i in range(self.n_modes-1,1,-1):
            samplelist.append(str(i))
            
        self.choice = wx.Choice(self.ModalTab, -1, (150, 150), choices = samplelist)
        self.Bind(wx.EVT_CHOICE, self.EvtChoice, self.choice)
        
        if self.dat[2] == 0:
            self.choice.SetSelection(0)
        else:
            for i in range(len(samplelist)-1,0,-1):
                if int(samplelist[i]) == self.dat[2]:
                    self.choice.SetSelection(i)

        hbox2 = wx.BoxSizer(wx.HORIZONTAL)
        quoteM = wx.StaticText(self.ModalTab,-1,' Number of modes to calculate: ')
        hbox2.Add(quoteM)
        hbox2.Add(self.choice)
        
        sizer2.Add(hbox2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self.ModalTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)
        sizer2.Add(wx.StaticText(self.ModalTab,-1,'   Parameters of the Jacobi method: '))

        Gsizer3 = wx.GridBagSizer(5,5)
        quotePJ1 = wx.StaticText(self.ModalTab,-1,'        Convergence tolerance: ')
        self.textPJ1 = wx.TextCtrl( self.ModalTab, -1, str(self.dat[3]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextPJ1, self.textPJ1)
        quotePJ2 = wx.StaticText(self.ModalTab,-1,'        Maximum number of iterations: ')
        self.textPJ2 = wx.TextCtrl( self.ModalTab, -1, str(self.dat[4]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextPJ2, self.textPJ2)
        Gsizer3.Add(quotePJ1,(0,0))
        Gsizer3.Add(self.textPJ1,(0,1))
        Gsizer3.Add(quotePJ2,(1,0))
        Gsizer3.Add(self.textPJ2,(1,1))

        sizer2.Add(Gsizer3, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self.ModalTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)        
        sizer2.Add(wx.StaticText(self.ModalTab,-1,'   Parameters of the Subspace Iteration method: '))
        
        Gsizer4 = wx.GridBagSizer(5,5)
        quotePS1 = wx.StaticText(self.ModalTab,-1,'        Convergence tolerance: ')
        self.textPS1 = wx.TextCtrl( self.ModalTab, -1, str(self.dat[5]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextPS1, self.textPS1)
        quotePS2 = wx.StaticText(self.ModalTab,-1,'        Maximum number of iterations: ')
        self.textPS2 = wx.TextCtrl( self.ModalTab, -1, str(self.dat[6]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextPS2, self.textPS2)
        Gsizer4.Add(quotePS1,(0,0))
        Gsizer4.Add(self.textPS1,(0,1))
        Gsizer4.Add(quotePS2,(1,0))
        Gsizer4.Add(self.textPS2,(1,1))
        
        sizer2.Add(Gsizer4, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        
        line = wx.StaticLine(self.ModalTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)        
        sizer2.Add(wx.StaticText(self.ModalTab,-1,'   Parameters of the QR algorithm: '))
        
        Gsizer5 = wx.GridBagSizer(5,5)
        quotePQ1 = wx.StaticText(self.ModalTab,-1,'        Convergence tolerance: ')
        self.textPQ1 = wx.TextCtrl( self.ModalTab, -1, str(self.dat[7]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextPQ1, self.textPQ1)
        quotePQ2 = wx.StaticText(self.ModalTab,-1,'        Maximum number of iterations: ')
        self.textPQ2 = wx.TextCtrl( self.ModalTab, -1, str(self.dat[8]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextPQ2, self.textPQ2)
        Gsizer5.Add(quotePQ1,(0,0))
        Gsizer5.Add(self.textPQ1,(0,1))
        Gsizer5.Add(quotePQ2,(1,0))
        Gsizer5.Add(self.textPQ2,(1,1))
        
        sizer2.Add(Gsizer5, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        line = wx.StaticLine(self.ModalTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)        
        sizer2.Add(wx.StaticText(self.ModalTab,-1,'   Parameters of the Inverse Iteration algorithm: '))
        
        Gsizer6 = wx.GridBagSizer(5,5)
        quotePI1 = wx.StaticText(self.ModalTab,-1,'        Convergence tolerance: ')
        self.textPI1 = wx.TextCtrl( self.ModalTab, -1, str(self.dat[9]), size=(50, -1) )
        self.Bind(wx.EVT_TEXT, self.EvtTextPQ1, self.textPQ1)
        quotePI2 = wx.StaticText(self.ModalTab,-1,'        Maximum number of iterations: ')
        self.textPI2 = wx.TextCtrl( self.ModalTab, -1, str(self.dat[10]), size=(50, -1) )
        self.Bind(wx.EVT_TEXT, self.EvtTextPI2, self.textPI2)
        Gsizer6.Add(quotePI1,(0,0))
        Gsizer6.Add(self.textPI1,(0,1))
        Gsizer6.Add(quotePI2,(1,0))
        Gsizer6.Add(self.textPI2,(1,1))
        
        sizer2.Add(Gsizer6, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        btnsizer = wx.StdDialogButtonSizer()
        
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
        
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(self.nb, 5, flag =  wx.ALL  | wx.ALIGN_CENTER_VERTICAL)
        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.StaticTab.SetSizer(sizer1)
        self.ModalTab.SetSizer(sizer2)

        self.SetSizer(sizer)
        sizer.Fit(self)

        self.OnGroup2Select( 0 )

        for radio in self.group1_ctrls:
            self.Bind(wx.EVT_RADIOBUTTON, self.OnGroup1Select, radio )

        for radio in self.group2_ctrls:
            self.Bind(wx.EVT_RADIOBUTTON, self.OnGroup2Select, radio )

    def OnGroup1Select( self, event ):
#         radio_selected = event.GetEventObject()
        radio = self.group1_ctrls[0]
        if radio.GetValue() == True:
            self.dat_out[0] = 1
        
        radio = self.group1_ctrls[1]    
        if radio.GetValue() == True:
            self.dat_out[0] = 2
            
    def OnGroup2Select( self, event ):
#         radio_selected = event.GetEventObject()
        radio = self.group2_ctrls[0]
        if radio.GetValue() == True:
            self.dat_out[1] = 1
            self.textPJ1.Enable(True)
            self.textPJ2.Enable(True)
            self.choice.SetSelection(0)
            self.choice.Disable()
        else:
            self.textPJ1.Enable(False)
            self.textPJ2.Enable(False)
            self.choice.Enable()

        radio = self.group2_ctrls[1]
        if radio.GetValue() == True:
            self.dat_out[1] = 2
            self.textPS1.Enable(True)
            self.textPS2.Enable(True)
            self.textPJ1.Enable(True)
            self.textPJ2.Enable(True)
        else:
            self.textPS1.Enable(False)
            self.textPS2.Enable(False)
            
        radio = self.group2_ctrls[2]
        if radio.GetValue() == True:
            self.dat_out[1] = 3
            self.textPQ1.Enable(True)
            self.textPQ2.Enable(True)
            self.textPI1.Enable(True)
            self.textPI2.Enable(True)
        else:
            self.textPQ1.Enable(False)
            self.textPQ2.Enable(False)
            self.textPI1.Enable(False)
            self.textPI2.Enable(False)
            
    def EvtTextPJ1(self,event):
        self.dat_out[3] = float(event.GetString())
        
    def EvtTextPJ2(self,event):
        self.dat_out[4] = int(event.GetString())
        
    def EvtTextPS1(self,event):
        self.dat_out[5] = float(event.GetString())
        
    def EvtTextPS2(self,event):
        self.dat_out[6] = int(event.GetString())
        
    def EvtTextPQ1(self,event):
        self.dat_out[7] = float(event.GetString())
        
    def EvtTextPQ2(self,event):
        self.dat_out[8] = int(event.GetString())
        
    def EvtTextPI1(self,event):
        self.dat_out[9] = float(event.GetString())
        
    def EvtTextPI2(self,event):
        self.dat_out[10] = int(event.GetString())
        
    def EvtChoice(self,event):
        val = event.GetString()
        if val == 'all' :
            self.dat_out[2] = 0
        else:
            self.dat_out[2] = int(val)

    def EvtAcc(self,event):
        self.dat_out[11] = int(event.GetString())

class Optimization_Options_DE(wx.Dialog):
    def __init__(self,vars_in,obj,constr,par):
        title = 'Optimization Options'
        
        self.vars = vars_in
        self.obj = obj
        self.constr = constr
        self.par = par
        
        self.vars_out = self.vars
        self.obj_out = self.obj
        self.constr_out = self.constr
        self.par_out = self.par

        wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)

        self.nb = wx.Notebook(self)
        
        self.ProblemTab = wx.Window(self.nb)
        self.ParameterTab = wx.Window(self.nb)
        self.ProblemTab.SetBackgroundColour("white")
        self.ParameterTab.SetBackgroundColour("white")
        
        self.nb.AddPage(self.ProblemTab, "Optimization Problem")
        self.nb.AddPage(self.ParameterTab, "Parameters")
    
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer1 = wx.BoxSizer(wx.VERTICAL)

        sizer1.AddSpacer(10)
        sizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Optimization Variables:  '))

#         self.scrolW = wx.ScrolledWindow(self.ProblemTab,-1,size=(700,200))
        sizer1.AddSpacer(10)
        self.Gscrsizer = wx.GridBagSizer(5,5)
        
        self.Txt11 = wx.StaticText(self.ProblemTab,-1,'  Element IDs  ')
        self.Txt12 = wx.StaticText(self.ProblemTab,-1,'  Lower bound  ')
        self.Txt13 = wx.StaticText(self.ProblemTab,-1,'  Upper bound  ')
        
        self.Gscrsizer.Add(wx.StaticText(self.ProblemTab,-1,'  Type  '),(0,0))
        self.Gscrsizer.Add(self.Txt11,(0,1),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
        self.Gscrsizer.Add(self.Txt12,(0,2),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
        self.Gscrsizer.Add(self.Txt13,(0,3),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
        self.Gscrsizer.Add(wx.StaticText(self.ProblemTab,-1,'  As one variable  '),(0,4),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
        
        self.types = ['Element cross section area','Position of node along X','Position of node along Y']
        
        self.combo1 = wx.ComboBox(self.ProblemTab, -1, size=(220, -1), choices=self.types, style=wx.CB_READONLY)
        self.combo1.SetValue(self.types[0])
        self.Bind(wx.EVT_COMBOBOX, self.EvtCombo1, self.combo1)
        self.Gscrsizer.Add(self.combo1,(1,0))
        
        self.textT0 = wx.TextCtrl( self.ProblemTab, -1, '', size=(120, -1)  )
        self.Gscrsizer.Add(self.textT0,(1,1),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)

        self.textT1 = wx.TextCtrl( self.ProblemTab, -1, '0.0', size=(50, -1)  )
        self.Gscrsizer.Add(self.textT1,(1,2),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)

        self.textT2 = wx.TextCtrl( self.ProblemTab, -1, '0.0', size=(50, -1)  )
        self.Gscrsizer.Add(self.textT2,(1,3),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)

        self.CheckOneV = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
        self.Gscrsizer.Add(self.CheckOneV,(1,4),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)

        sizer1.Add(self.Gscrsizer)
        
        sizer1.AddSpacer(10)
        Addbtn = wx.Button(self.ProblemTab, label="Add")
        Addbtn.Bind(wx.EVT_BUTTON, self.add_variable)
        sizer1.Add(Addbtn,flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
        
        self.list_index = 0
        self.Nvar = 0
        sizer1.AddSpacer(10)
        self.list_ctrl = wx.ListCtrl(self.ProblemTab, size=(720,150),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
        self.list_ctrl.InsertColumn(0, 'Variable ID', width=100)
        self.list_ctrl.InsertColumn(1, 'Type', width=100)
        self.list_ctrl.InsertColumn(2, 'Element/Node IDs', width=150)
        self.list_ctrl.InsertColumn(3, 'Upper Bound/dx1/dy1', width=170)
        self.list_ctrl.InsertColumn(4, 'Upper Bound/dy1/dy1', width=170)
        sizer1.Add(self.list_ctrl)

        sizer1.AddSpacer(10)
        Delbtn = wx.Button(self.ProblemTab, label="Delete")
        Delbtn.Bind(wx.EVT_BUTTON, self.delete_variable)
        sizer1.Add(Delbtn,flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
        
        sizer1.AddSpacer(10)
        
        line = wx.StaticLine(self.ProblemTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer1.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)       
        
        sizer1.AddSpacer(10)
        sizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Objective function:  '))
        sizer1.AddSpacer(10)
        sizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Minimize  '))
#         sizer1.AddSpacer(3)
        
        self.group1_ctrls = []
        radio1 = wx.RadioButton( self.ProblemTab, -1, " External Work  ", style = wx.RB_GROUP )
        radio2 = wx.RadioButton( self.ProblemTab, -1, " Total Mass  " )
        
        self.group1_ctrls.append((radio1))
        self.group1_ctrls.append((radio2))
        
        if self.obj == 1:
            radio1.SetValue(True)
        elif self.obj == 2:
            radio2.SetValue(True)
        
        Gsizer = wx.GridBagSizer(5,5)
        Gsizer.Add(radio1,(0,0))
        Gsizer.Add(radio2,(0,1))
        sizer1.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        for radio in self.group1_ctrls:
            self.Bind(wx.EVT_RADIOBUTTON, self.OnGroup1Select, radio )

        line = wx.StaticLine(self.ProblemTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer1.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)  

        sizer1.AddSpacer(10)
        sizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Constraints:  '))
        sizer1.AddSpacer(10)

        
        Gsizer1 = wx.GridBagSizer(5,5)
        Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Minimum'),(0,2))
        Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Maximum'),(0,3))
        Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  External Work:  '),(1,0))
        Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Total Mass:  '),(2,0))
        Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Maximum Stress:  '),(3,0))
        Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  1st eigen-frequency:  '),(4,0))

        self.CheckC1 = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
        self.Bind(wx.EVT_CHECKBOX, self.on_CheckC1, self.CheckC1)
        Gsizer1.Add(self.CheckC1,(1,1))
        
        self.CheckC2 = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
        self.Bind(wx.EVT_CHECKBOX, self.on_CheckC2, self.CheckC2)
        Gsizer1.Add(self.CheckC2,(2,1))
        
        self.CheckC3 = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
        self.Bind(wx.EVT_CHECKBOX, self.on_CheckC3, self.CheckC3)
        Gsizer1.Add(self.CheckC3,(3,1))
        
        self.CheckC4 = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
        self.Bind(wx.EVT_CHECKBOX, self.on_CheckC4, self.CheckC4)
        Gsizer1.Add(self.CheckC4,(4,1))
        
        self.textC11 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[0][1]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextC11, self.textC11)
        Gsizer1.Add(self.textC11,(1,2))

        self.textC12 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[0][2]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextC12, self.textC12)
        Gsizer1.Add(self.textC12,(1,3))
        
        self.textC21 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[1][1]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextC21, self.textC21)
        Gsizer1.Add(self.textC21,(2,2))

        self.textC22 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[1][2]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextC22, self.textC22)
        Gsizer1.Add(self.textC22,(2,3))
        
        self.textC31 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[2][1]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextC31, self.textC31)
        Gsizer1.Add(self.textC31,(3,2))

        self.textC32 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[2][2]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextC32, self.textC32)
        Gsizer1.Add(self.textC32,(3,3))
        
        self.textC41 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[3][1]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextC41, self.textC41)
        Gsizer1.Add(self.textC41,(4,2))

        self.textC42 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[3][2]), size=(50, -1)  )
        self.Bind(wx.EVT_TEXT, self.EvtTextC42, self.textC42)
        Gsizer1.Add(self.textC42,(4,3))
        
        if self.constr[0][0] == 0:
            self.textC11.Enable(False)
            self.textC12.Enable(False)
            self.CheckC1.SetValue(False)
        else:
            self.textC11.Enable(True)
            self.textC12.Enable(True)
            self.CheckC1.SetValue(True)

        if self.constr[1][0] == 0:
            self.textC21.Enable(False)
            self.textC22.Enable(False)
            self.CheckC2.SetValue(False)
        else:
            self.textC21.Enable(True)
            self.textC22.Enable(True)
            self.CheckC2.SetValue(True)

        if self.constr[2][0] == 0:
            self.textC31.Enable(False)
            self.textC32.Enable(False)
            self.CheckC3.SetValue(False)
        else:
            self.textC31.Enable(True)
            self.textC32.Enable(True)
            self.CheckC3.SetValue(True)

        if self.constr[3][0] == 0:
            self.textC41.Enable(False)
            self.textC42.Enable(False)
            self.CheckC4.SetValue(False)
        else:
            self.textC41.Enable(True)
            self.textC42.Enable(True)
            self.CheckC4.SetValue(True)

        if radio1.GetValue() == True:
            self.CheckC1.Enable(False)
            self.textC11.Enable(False)
            self.textC12.Enable(False)
        else:
            self.CheckC2.Enable(False)
            self.textC21.Enable(False)
            self.textC22.Enable(False)

        sizer1.Add(Gsizer1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)


        sizer2 = wx.BoxSizer(wx.VERTICAL)
         
        sizer2.AddSpacer(10)
        sizer2.Add(wx.StaticText(self.ParameterTab,-1,'    Parameters of the Differential Evolution algorithm   '))
        sizer2.AddSpacer(5)
        line = wx.StaticLine(self.ParameterTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)  
# -----------------------------------------------         
        Gsizer2 = wx.GridBagSizer(5,5)
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Population size:   '),(0,0))
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Total number of iterations:   '),(1,0))
                
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   NP =   '),(0,1))
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Itermax =  '),(1,1))        

        self.NP = wx.TextCtrl( self.ParameterTab, -1, str(int(self.par[0])), size=(70, -1))
        self.Bind(wx.EVT_TEXT, self.on_NP, self.NP)
        Gsizer2.Add(self.NP,(0,2))

        self.Itermax = wx.TextCtrl( self.ParameterTab, -1, str(int(self.par[1])), size=(70, -1))
        self.Bind(wx.EVT_TEXT, self.on_Itermax, self.Itermax)
        Gsizer2.Add(self.Itermax,(1,2))
# ----------------------------------------------- 
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Mutations scaling factor:   '),(3,0))

        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   F_XC =   '),(3,1))
        
        self.F_XC = ['fixed','random [0, 1]','random [-1, 1]']        
        self.combo2 = wx.ComboBox(self.ParameterTab, -1, size=(100, -1), choices=self.F_XC, style=wx.CB_READONLY)
        self.combo2.SetValue(self.F_XC[0])
        self.Bind(wx.EVT_COMBOBOX, self.EvtCombo2, self.combo2)
        Gsizer2.Add(self.combo2,(3,2))

        self.T_F_XC = wx.TextCtrl( self.ParameterTab, -1, str((self.par[2])), size=(70, -1))
        self.Bind(wx.EVT_TEXT, self.on_T_F_XC, self.T_F_XC)
        self.T_F_XC.Enable(True)
        Gsizer2.Add(self.T_F_XC,(3,3))       
# -----------------------------------------------        
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Crossover factor:   '),(4,0))

        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   CR_XC =   '),(4,1))        

        self.CR_XC = wx.TextCtrl( self.ParameterTab, -1, str(self.par[3]), size=(70, -1))
        self.Bind(wx.EVT_TEXT, self.on_CR_XC, self.CR_XC)
        Gsizer2.Add(self.CR_XC,(4,2))
# -----------------------------------------------         
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Mutation strategy:   '),(5,0))

        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   strategy =   '),(5,1)) 

        self.strategy = ['1','2','3','4','5','6']        
        self.combo3 = wx.ComboBox(self.ParameterTab, -1, size=(100, -1), choices=self.strategy, style=wx.CB_READONLY)
        self.combo3.SetValue(self.strategy[5])
        self.Bind(wx.EVT_COMBOBOX, self.EvtCombo3, self.combo3)
        Gsizer2.Add(self.combo3,(5,2))            
# -----------------------------------------------  
        self.stat1 = wx.StaticText(self.ParameterTab,-1,'   Combined factor:   ')
        Gsizer2.Add(self.stat1,(6,0))
        
        self.stat2 = wx.StaticText(self.ParameterTab,-1,'   F_CR =   ')
        Gsizer2.Add(self.stat2,(6,1))

        self.F_CR = ['fixed','random [0, 1]']        
        self.combo4 = wx.ComboBox(self.ParameterTab, -1, size=(100, -1), choices=self.F_CR, style=wx.CB_READONLY)
        self.combo4.SetValue(self.F_CR[1])
        self.Bind(wx.EVT_COMBOBOX, self.EvtCombo4, self.combo4)
        Gsizer2.Add(self.combo4,(6,2))

        self.T_F_CR = wx.TextCtrl( self.ParameterTab, -1, str((self.par[4])), size=(70, -1))
        self.Bind(wx.EVT_TEXT, self.on_T_F_CR, self.T_F_CR)
        self.T_F_CR.Enable(False)
        Gsizer2.Add(self.T_F_CR,(6,3))
# -----------------------------------------------         
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Save optimization results:   '),(8,0))
        
        self.CheckSave = wx.CheckBox(self.ParameterTab, -1,'', style=wx.ALIGN_RIGHT)
        self.Bind(wx.EVT_CHECKBOX, self.on_CheckSave, self.CheckSave)
        Gsizer2.Add(self.CheckSave,(8,1),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
        
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   every   '),(8,2))

        self.Trefresh = wx.TextCtrl( self.ParameterTab, -1, str(int(self.par[6])), size=(70, -1))
        self.Bind(wx.EVT_TEXT, self.on_Trefresh, self.Trefresh)
        self.Trefresh.Enable(False)
        Gsizer2.Add(self.Trefresh,(8,3))
        
        Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   iterations'),(8,4))

# -----------------------------------------------  
        sizer2.Add(Gsizer2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        
        
        sizer2.AddSpacer(10)
        line = wx.StaticLine(self.ParameterTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 2)  
        sizer2.AddSpacer(5)
        sizer2.Add(wx.StaticText(self.ParameterTab,-1,'    Parameters of the penalty-based constraint enforcemet algorithm   '))
        sizer2.AddSpacer(5)
        line = wx.StaticLine(self.ParameterTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)  

        Gsizer3 = wx.GridBagSizer(1,3)

        Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Penalty factor :   '),(0,0))
        
        Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Rp =   '),(0,1))

        self.TRp = wx.TextCtrl( self.ParameterTab, -1, str(int(self.par[11])), size=(70, -1))
        self.Bind(wx.EVT_TEXT, self.on_TRp, self.TRp)
        Gsizer3.Add(self.TRp,(0,2))        
        
        
        sizer2.Add(Gsizer3, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)

        btnsizer = wx.StdDialogButtonSizer()
          
        if wx.Platform != "__WXMSW__":
            btn = wx.ContextHelpButton(self)
            btnsizer.AddButton(btn)
          
        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)
  
        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()
  
        sizer.Add(self.nb, 5, flag =  wx.ALL  | wx.ALIGN_CENTER_VERTICAL)
        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
 
        self.ProblemTab.SetSizer(sizer1)
        self.ParameterTab.SetSizer(sizer2)

        self.SetSizer(sizer)
        sizer.Fit(self)
        
        self.update_list()

    def OnGroup1Select( self, event ):
#         radio_selected = event.GetEventObject()
        radio = self.group1_ctrls[0]
        if radio.GetValue() == True:
            self.obj_out = 1
#             self.CheckC1.SetValue(False)
            self.CheckC2.Enable(True)
            self.CheckC1.Enable(False)
            
            self.textC11.Enable(False)
            self.textC12.Enable(False)
            if self.CheckC2.GetValue() == True:       
                self.textC21.Enable(True)
                self.textC22.Enable(True)     
        
        radio = self.group1_ctrls[1]    
        if radio.GetValue() == True:
            self.obj_out = 2
#             self.CheckC2.SetValue(False)
            self.CheckC2.Enable(False)
            self.CheckC1.Enable(True)
           
            self.textC21.Enable(False)
            self.textC22.Enable(False)
            if self.CheckC1.GetValue() == True:
                self.textC11.Enable(True)
                self.textC12.Enable(True)
            
    def on_CheckC1(self,event):
        if event.IsChecked():
            self.constr_out[0][0] = 1
            self.textC11.Enable(True)
            self.textC12.Enable(True)
        else:
            self.constr_out[0][0] = 0
            self.textC11.Enable(False)
            self.textC12.Enable(False)
    
    def on_CheckC2(self,event):
        if event.IsChecked():
            self.constr_out[1][0] = 2
            self.textC21.Enable(True)
            self.textC22.Enable(True)
        else:
            self.constr_out[1][0] = 0
            self.textC21.Enable(False)
            self.textC22.Enable(False)
    
    def on_CheckC3(self,event):
        if event.IsChecked():
            self.constr_out[2][0] = 3
            self.textC31.Enable(True)
            self.textC32.Enable(True)
        else:
            self.constr_out[2][0] = 0
            self.textC31.Enable(False)
            self.textC32.Enable(False)
    
    def on_CheckC4(self,event):
        if event.IsChecked():
            self.constr_out[3][0] = 4
            self.textC41.Enable(True)
            self.textC42.Enable(True)
        else:
            self.constr_out[3][0] = 0
            self.textC41.Enable(False)
            self.textC42.Enable(False)

    def on_CheckOneV(self,event):
        pass

    def EvtCombo1(self,event):
        if self.combo1.GetValue() == self.types[0]:
            self.Txt11.SetLabel('  Element IDs  ')
            self.Txt12.SetLabel('  Lower bound  ')
            self.Txt13.SetLabel('  Upper bound  ')
            self.Gscrsizer.Layout()
        elif self.combo1.GetValue() == self.types[1]:
            self.Txt11.SetLabel('  Node IDs  ')
            self.Txt12.SetLabel('  dx1  ')
            self.Txt13.SetLabel('  dx2  ')
#             self.Txt12.Wrap(160)
#             self.Txt13.Wrap(160)
            self.Gscrsizer.Layout()
        elif self.combo1.GetValue() == self.types[2]:
            self.Txt11.SetLabel('  Node IDs  ')
            self.Txt12.SetLabel('  dy1  ')
            self.Txt13.SetLabel('  dy2  ')
            self.Gscrsizer.Layout()

    def GetSelectedItems(self):
        selection = []
        index = self.list_ctrl.GetFirstSelected()
        selection.append(index)
        while len(selection) != self.list_ctrl.GetSelectedItemCount() and index != -1:
            index = self.list_ctrl.GetNextSelected(index)
            selection.append(index)
            
        return selection

    def update_list(self):
        self.list_ctrl.DeleteAllItems()
        for i in range(0,len(self.vars)):
            self.list_ctrl.InsertStringItem(i, str(self.vars[i][0]))
            self.list_ctrl.SetStringItem(i, 1, str(self.types[int(self.vars[i][1])]))
            self.list_ctrl.SetStringItem(i, 2, str(self.vars[i][2]))
            self.list_ctrl.SetStringItem(i, 3, str(self.vars[i][3]))
            self.list_ctrl.SetStringItem(i, 4, str(self.vars[i][4]))           

    def renumber_list(self):
        print(self.vars)
        indct = np.zeros(len(self.vars))
        for i in range(0,len(self.vars)-1):
            if self.vars[i][0] == self.vars[i+1][0]:
                indct[i] = 1
        if self.vars[-1][0] == self.vars[-2][0]:
            indct[-1] = 1
        
        k = 1
        for i in range(0,len(self.vars)):
            self.vars[i][0] = k
            if indct[i] == 0:
                k = k+1
        
        self.Nvar = 0        
        for i in range(0,len(self.vars)):
            if self.Nvar < self.vars[i][0]:
                self.Nvar = self.vars[i][0]
                
        print(self.Nvar)

    def delete_variable(self,event):
        IDs = self.GetSelectedItems()
        print(IDs)
        if IDs[0] != -1:
            for i in reversed(IDs):
                del self.vars[i]
                self.list_index = self.list_index - 1

        self.renumber_list()
        self.update_list()
        
    def add_variable(self,event):
        if self.textT0.GetValue() == '':
            return
        IDs_tmp1 = self.textT0.GetValue().split(",")
        IDs_tmp2 = self.textT0.GetValue().split(":")
        if len(IDs_tmp2) > 1:
            IDs = range(int(IDs_tmp2[0]),int(IDs_tmp2[1])+1)
            IDs = [str(i) for i in IDs]
        else:
            IDs =  IDs_tmp1
            
#        IDs = self.textT0.GetValue().split(",")
#        print(IDs)
        if self.CheckOneV.GetValue() == True:
            self.Nvar = self.Nvar + 1
        for i in range(0,len(IDs)):
            self.list_ctrl.InsertStringItem(self.list_index, str(self.Nvar))
            if self.CheckOneV.GetValue() == False:
                self.Nvar = self.Nvar + 1
            self.list_ctrl.SetStringItem(self.list_index, 1, self.combo1.GetValue())   
            self.list_ctrl.SetStringItem(self.list_index, 2, IDs[i])
            self.list_ctrl.SetStringItem(self.list_index, 3, self.textT1.GetValue())
            self.list_ctrl.SetStringItem(self.list_index, 4, self.textT2.GetValue())
            if self.combo1.GetValue() == self.types[0]:
                typ = 0
            elif self.combo1.GetValue() == self.types[1]:
                typ = 1
            elif self.combo1.GetValue() == self.types[2]:
                typ = 2
            self.vars.append([self.Nvar,typ,int(IDs[i]),float(self.textT1.GetValue()),float(self.textT2.GetValue())])
            self.list_index = self.list_index + 1     
        self.textT0.SetValue('')
        self.textT1.SetValue('0.0')
        self.textT2.SetValue('0.0')
        self.renumber_list()
        self.update_list()

    def on_NP(self,event):
        self.par_out[0] = int(event.GetString())
    
    def on_Itermax(self,event):
        self.par_out[1] = int(event.GetString())

    def on_T_F_XC(self,event):
        self.par_out[2] = float(event.GetString())

    def on_CR_XC(self,event):
        self.par_out[3] = float(event.GetString())

    def on_T_F_CR(self,event):
        self.par_out[4] = float(event.GetString())

    def EvtCombo3(self,event):
        self.stat1.Show(False)
        self.stat2.Show(False)
        self.stat1.SetForegroundColour((211,211,211)) # set text color
        self.stat2.SetForegroundColour((211,211,211))
        self.stat1.Show(True)
        self.stat2.Show(True)
        self.combo4.Enable(False)
        self.T_F_CR.Enable(False)
        if self.combo3.GetValue() == self.strategy[0]:
            self.par_out[5] = 1
        elif self.combo3.GetValue() == self.strategy[1]:
            self.par_out[5] = 2
        elif self.combo3.GetValue() == self.strategy[2]:
            self.par_out[5] = 3
        elif self.combo3.GetValue() == self.strategy[3]:
            self.par_out[5] = 4
        elif self.combo3.GetValue() == self.strategy[4]:
            self.par_out[5] = 5
        elif self.combo3.GetValue() == self.strategy[5]:
            self.par_out[5] = 6
            self.stat1.Show(False)
            self.stat2.Show(False)
            self.stat1.SetForegroundColour((0,0,0)) # set text color
            self.stat2.SetForegroundColour((0,0,0))
            self.stat1.Show(True)
            self.stat2.Show(True)
            self.combo4.Enable(True)
            if self.combo4.GetValue() == self.F_CR[0]:
                self.T_F_CR.Enable(True)
            elif self.combo4.GetValue() == self.F_CR[1]:
                self.T_F_CR.Enable(False)

    def on_Trefresh(self,event):
        self.par_out[6] = float(event.GetString())


    def EvtCombo2(self,event):
        if self.combo2.GetValue() == self.F_XC[0]:
            self.par_out[8] = 0
            self.T_F_XC.Enable(True)
        elif self.combo2.GetValue() == self.F_XC[1]:
            self.par_out[8] = 1
            self.T_F_XC.Enable(False)
        elif self.combo2.GetValue() == self.F_XC[2]:
            self.par_out[8] = 2
            self.T_F_XC.Enable(False)

    def EvtCombo4(self,event):
        if self.combo4.GetValue() == self.F_CR[0]:
            self.par_out[9] = 0
            self.T_F_CR.Enable(True)
        elif self.combo4.GetValue() == self.F_CR[1]:
            self.par_out[9] = 1
            self.T_F_CR.Enable(False)

    def on_CheckSave(self,event):
        if event.IsChecked():
            self.par_out[10] = 1
            self.Trefresh.Enable(True)
        else:
            self.par_out[10] = 0
            self.Trefresh.Enable(False)

    def on_TRp(self,event):
        self.par_out[11] = float(event.GetString())
        
    def EvtTextC11(self,event):
        self.constr_out[0][1] = float(event.GetString())
 
    def EvtTextC12(self,event):
        self.constr_out[0][2] = float(event.GetString())
         
    def EvtTextC21(self,event):
        self.constr_out[1][1] = float(event.GetString())
         
    def EvtTextC22(self,event):
        self.constr_out[1][2] = float(event.GetString())
 
    def EvtTextC31(self,event):
        self.constr_out[2][1] = float(event.GetString())
         
    def EvtTextC32(self,event):
        self.constr_out[2][2] = float(event.GetString())
         
    def EvtTextC41(self,event):
        self.constr_out[3][1] = float(event.GetString())
         
    def EvtTextC42(self,event):
        self.constr_out[3][2] = float(event.GetString())
        
        
        
# class Optimization_Options_evolu(wx.Dialog):
#     def __init__(self,vars_in,obj,constr,par):
#         title = 'Optimization Options'
#         
#         self.vars = vars_in
#         self.obj = obj
#         self.constr = constr
#         self.par = par
#         
#         self.vars_out = self.vars
#         self.obj_out = self.obj
#         self.constr_out = self.constr
#         self.par_out = self.par
# 
#         wx.Dialog.__init__(self, None, -1, title, size=wx.DefaultSize, pos=wx.DefaultPosition, style=wx.DEFAULT_DIALOG_STYLE)
# 
#         self.nb = wx.Notebook(self)
#         
#         self.ProblemTab = wx.Window(self.nb)
#         self.ParameterTab = wx.Window(self.nb)
#         self.ProblemTab.SetBackgroundColour("white")
#         self.ParameterTab.SetBackgroundColour("white")
#         
#         self.nb.AddPage(self.ProblemTab, "Optimization Problem")
#         self.nb.AddPage(self.ParameterTab, "Parameters")
#     
#         
#         sizer = wx.BoxSizer(wx.VERTICAL)
#         sizer1 = wx.BoxSizer(wx.VERTICAL)
# 
#         sizer1.AddSpacer(10)
#         sizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Optimization Variables:  '))
# 
# #         self.scrolW = wx.ScrolledWindow(self.ProblemTab,-1,size=(700,200))
#         sizer1.AddSpacer(10)
#         self.Gscrsizer = wx.GridBagSizer(5,5)
#         
#         self.Txt11 = wx.StaticText(self.ProblemTab,-1,'  Element IDs  ')
#         self.Txt12 = wx.StaticText(self.ProblemTab,-1,'  Lower bound  ')
#         self.Txt13 = wx.StaticText(self.ProblemTab,-1,'  Upper bound  ')
#         
#         self.Gscrsizer.Add(wx.StaticText(self.ProblemTab,-1,'  Type  '),(0,0))
#         self.Gscrsizer.Add(self.Txt11,(0,1),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
#         self.Gscrsizer.Add(self.Txt12,(0,2),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
#         self.Gscrsizer.Add(self.Txt13,(0,3),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
#         self.Gscrsizer.Add(wx.StaticText(self.ProblemTab,-1,'  As one variable  '),(0,4),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
#         
#         self.types = ['Element cross section area','Position of node along X','Position of node along Y']
#         
#         self.combo1 = wx.ComboBox(self.ProblemTab, -1, size=(220, -1), choices=self.types, style=wx.CB_READONLY)
#         self.combo1.SetValue(self.types[0])
#         self.Bind(wx.EVT_COMBOBOX, self.EvtCombo1, self.combo1)
#         self.Gscrsizer.Add(self.combo1,(1,0))
#         
#         self.textT0 = wx.TextCtrl( self.ProblemTab, -1, '', size=(120, -1)  )
#         self.Gscrsizer.Add(self.textT0,(1,1),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
# 
#         self.textT1 = wx.TextCtrl( self.ProblemTab, -1, '0.0', size=(50, -1)  )
#         self.Gscrsizer.Add(self.textT1,(1,2),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
# 
#         self.textT2 = wx.TextCtrl( self.ProblemTab, -1, '0.0', size=(50, -1)  )
#         self.Gscrsizer.Add(self.textT2,(1,3),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
# 
#         self.CheckOneV = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
#         self.Gscrsizer.Add(self.CheckOneV,(1,4),flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
# 
#         sizer1.Add(self.Gscrsizer)
#         
#         sizer1.AddSpacer(10)
#         Addbtn = wx.Button(self.ProblemTab, label="Add")
#         Addbtn.Bind(wx.EVT_BUTTON, self.add_variable)
#         sizer1.Add(Addbtn,flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
#         
#         self.list_index = 0
#         self.Nvar = 0
#         sizer1.AddSpacer(10)
#         self.list_ctrl = wx.ListCtrl(self.ProblemTab, size=(720,150),style=wx.LC_REPORT|wx.BORDER_DEFAULT)
#         self.list_ctrl.InsertColumn(0, 'Variable ID', width=100)
#         self.list_ctrl.InsertColumn(1, 'Type', width=100)
#         self.list_ctrl.InsertColumn(2, 'Element/Node IDs', width=150)
#         self.list_ctrl.InsertColumn(3, 'Upper Bound/dx1/dy1', width=170)
#         self.list_ctrl.InsertColumn(4, 'Upper Bound/dy1/dy1', width=170)
#         sizer1.Add(self.list_ctrl)
# 
#         sizer1.AddSpacer(10)
#         Delbtn = wx.Button(self.ProblemTab, label="Delete")
#         Delbtn.Bind(wx.EVT_BUTTON, self.delete_variable)
#         sizer1.Add(Delbtn,flag=wx.ALL|wx.ALIGN_CENTER_HORIZONTAL)
#         
#         sizer1.AddSpacer(10)
#         
#         line = wx.StaticLine(self.ProblemTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#         sizer1.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)       
#         
#         sizer1.AddSpacer(10)
#         sizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Objective function:  '))
#         sizer1.AddSpacer(10)
#         sizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Minimize  '))
# #         sizer1.AddSpacer(3)
#         
#         self.group1_ctrls = []
#         radio1 = wx.RadioButton( self.ProblemTab, -1, " External Work  ", style = wx.RB_GROUP )
#         radio2 = wx.RadioButton( self.ProblemTab, -1, " Total Mass  " )
#         
#         self.group1_ctrls.append((radio1))
#         self.group1_ctrls.append((radio2))
#         
#         if self.obj == 1:
#             radio1.SetValue(True)
#         elif self.obj == 2:
#             radio2.SetValue(True)
#         
#         Gsizer = wx.GridBagSizer(5,5)
#         Gsizer.Add(radio1,(0,0))
#         Gsizer.Add(radio2,(0,1))
#         sizer1.Add(Gsizer, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
# 
#         for radio in self.group1_ctrls:
#             self.Bind(wx.EVT_RADIOBUTTON, self.OnGroup1Select, radio )
# 
#         line = wx.StaticLine(self.ProblemTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#         sizer1.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)  
# 
#         sizer1.AddSpacer(10)
#         sizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Constraints:  '))
#         sizer1.AddSpacer(10)
# 
#         
#         Gsizer1 = wx.GridBagSizer(5,5)
#         Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Minimum'),(0,2))
#         Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Maximum'),(0,3))
#         Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  External Work:  '),(1,0))
#         Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Total Mass:  '),(2,0))
#         Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  Maximum Stress:  '),(3,0))
#         Gsizer1.Add(wx.StaticText(self.ProblemTab,-1,'  1st eigen-frequency:  '),(4,0))
# 
#         self.CheckC1 = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
#         self.Bind(wx.EVT_CHECKBOX, self.on_CheckC1, self.CheckC1)
#         Gsizer1.Add(self.CheckC1,(1,1))
#         
#         self.CheckC2 = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
#         self.Bind(wx.EVT_CHECKBOX, self.on_CheckC2, self.CheckC2)
#         Gsizer1.Add(self.CheckC2,(2,1))
#         
#         self.CheckC3 = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
#         self.Bind(wx.EVT_CHECKBOX, self.on_CheckC3, self.CheckC3)
#         Gsizer1.Add(self.CheckC3,(3,1))
#         
#         self.CheckC4 = wx.CheckBox(self.ProblemTab, -1,'', style=wx.ALIGN_RIGHT)
#         self.Bind(wx.EVT_CHECKBOX, self.on_CheckC4, self.CheckC4)
#         Gsizer1.Add(self.CheckC4,(4,1))
#         
#         self.textC11 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[0][1]), size=(50, -1)  )
#         self.Bind(wx.EVT_TEXT, self.EvtTextC11, self.textC11)
#         Gsizer1.Add(self.textC11,(1,2))
# 
#         self.textC12 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[0][2]), size=(50, -1)  )
#         self.Bind(wx.EVT_TEXT, self.EvtTextC12, self.textC12)
#         Gsizer1.Add(self.textC12,(1,3))
#         
#         self.textC21 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[1][1]), size=(50, -1)  )
#         self.Bind(wx.EVT_TEXT, self.EvtTextC21, self.textC21)
#         Gsizer1.Add(self.textC21,(2,2))
# 
#         self.textC22 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[1][2]), size=(50, -1)  )
#         self.Bind(wx.EVT_TEXT, self.EvtTextC22, self.textC22)
#         Gsizer1.Add(self.textC22,(2,3))
#         
#         self.textC31 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[2][1]), size=(50, -1)  )
#         self.Bind(wx.EVT_TEXT, self.EvtTextC31, self.textC31)
#         Gsizer1.Add(self.textC31,(3,2))
# 
#         self.textC32 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[2][2]), size=(50, -1)  )
#         self.Bind(wx.EVT_TEXT, self.EvtTextC32, self.textC32)
#         Gsizer1.Add(self.textC32,(3,3))
#         
#         self.textC41 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[3][1]), size=(50, -1)  )
#         self.Bind(wx.EVT_TEXT, self.EvtTextC41, self.textC41)
#         Gsizer1.Add(self.textC41,(4,2))
# 
#         self.textC42 = wx.TextCtrl( self.ProblemTab, -1, str(self.constr[3][2]), size=(50, -1)  )
#         self.Bind(wx.EVT_TEXT, self.EvtTextC42, self.textC42)
#         Gsizer1.Add(self.textC42,(4,3))
#         
#         if self.constr[0][0] == 0:
#             self.textC11.Enable(False)
#             self.textC12.Enable(False)
#             self.CheckC1.SetValue(False)
#         else:
#             self.textC11.Enable(True)
#             self.textC12.Enable(True)
#             self.CheckC1.SetValue(True)
# 
#         if self.constr[1][0] == 0:
#             self.textC21.Enable(False)
#             self.textC22.Enable(False)
#             self.CheckC2.SetValue(False)
#         else:
#             self.textC21.Enable(True)
#             self.textC22.Enable(True)
#             self.CheckC2.SetValue(True)
# 
#         if self.constr[2][0] == 0:
#             self.textC31.Enable(False)
#             self.textC32.Enable(False)
#             self.CheckC3.SetValue(False)
#         else:
#             self.textC31.Enable(True)
#             self.textC32.Enable(True)
#             self.CheckC3.SetValue(True)
# 
#         if self.constr[3][0] == 0:
#             self.textC41.Enable(False)
#             self.textC42.Enable(False)
#             self.CheckC4.SetValue(False)
#         else:
#             self.textC41.Enable(True)
#             self.textC42.Enable(True)
#             self.CheckC4.SetValue(True)
# 
#         if radio1.GetValue() == True:
#             self.CheckC1.Enable(False)
#             self.textC11.Enable(False)
#             self.textC12.Enable(False)
#         else:
#             self.CheckC2.Enable(False)
#             self.textC21.Enable(False)
#             self.textC22.Enable(False)
# 
#         sizer1.Add(Gsizer1, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
# 
# 
#         sizer2 = wx.BoxSizer(wx.VERTICAL)
#          
#         sizer2.AddSpacer(10)
#         sizer2.Add(wx.StaticText(self.ParameterTab,-1,'    Parameters of the evolution strategy algorithm   '))
#         sizer2.AddSpacer(5)
#         line = wx.StaticLine(self.ParameterTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#         sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)  
#          
#         Gsizer2 = wx.GridBagSizer(5,5)
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Number of optimization chains:   '),(0,0))
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Number of offspring points corresponding to a parent point:   '),(1,0))
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Maximum number of generations per chain:   '),(2,0))
# 
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   N_chain =   '),(0,1))
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   N_sapo =  '),(1,1))
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Maxsim =   '),(2,1))
# 
#         self.Tnchain = wx.TextCtrl( self.ParameterTab, -1, str(int(self.par[0])), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_Tnchain, self.Tnchain)
#         Gsizer2.Add(self.Tnchain,(0,2))
# 
#         self.Tsapo = wx.TextCtrl( self.ParameterTab, -1, str(int(self.par[1])), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_Tsapo, self.Tsapo)
#         Gsizer2.Add(self.Tsapo,(1,2))
# 
#         self.Tmaxsim = wx.TextCtrl( self.ParameterTab, -1, str(int(self.par[2])), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_Tmaxsim, self.Tmaxsim)
#         Gsizer2.Add(self.Tmaxsim,(2,2))       
# 
# #         sizer2.AddSpacer(30)
# 
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Minimum mutation distance from parent point:   '),(4,0))
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Maximum mutation distance from parent point:   '),(5,0))
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Shape of the offspring generation subspace:   '),(6,0))
#         
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   D_min =   '),(4,1))
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   D_max =   '),(5,1))
#         Gsizer2.Add(wx.StaticText(self.ParameterTab,-1,'   Shape_subsp =   '),(6,1))
# 
#         self.Tsisusp1 = wx.TextCtrl( self.ParameterTab, -1, str(self.par[3]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_Tsisusp1, self.Tsisusp1)
#         Gsizer2.Add(self.Tsisusp1,(4,2))
#         
#         self.Tsisusp2 = wx.TextCtrl( self.ParameterTab, -1, str(self.par[4]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_Tsisusp2, self.Tsisusp2)
#         Gsizer2.Add(self.Tsisusp2,(5,2))
# 
#         self.Shape_subsp = ['hyper-rectangle','hyper-ellipse']
#         
#         self.combo2 = wx.ComboBox(self.ParameterTab, -1, size=(150, -1), choices=self.Shape_subsp, style=wx.CB_READONLY)
#         self.combo2.SetValue(self.Shape_subsp[0])
#         self.Bind(wx.EVT_COMBOBOX, self.EvtCombo2, self.combo2)
#         Gsizer2.Add(self.combo2,(6,2))
#        
#         sizer2.Add(Gsizer2, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
#         
#         
#         sizer2.AddSpacer(10)
#         line = wx.StaticLine(self.ParameterTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#         sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 2)  
#         sizer2.AddSpacer(5)
#         sizer2.Add(wx.StaticText(self.ParameterTab,-1,'    Parameters of the penalty-based constraint enforcemet algorithm   '))
#         sizer2.AddSpacer(5)
#         line = wx.StaticLine(self.ParameterTab, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
#         sizer2.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)  
# 
#         Gsizer3 = wx.GridBagSizer(5,5)
#         
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Tolerance in the variable space:   '),(0,0))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Tolerance in the result space:   '),(1,0))
#         
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Initial penalty factor (Rp):   '),(3,0))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Multiplicative increment of the Rp:   '),(4,0))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Maximum Rp before increasing Tol_x:   '),(5,0))
#         
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Multiplicative increment of Tol_x:   '),(7,0))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Maximum Tol_x to abort optimization:   '),(8,0))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Core optimization convergence tolerance:   '),(10,0))
# 
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Tol_x =   '),(0,1))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Tol_z =  '),(1,1))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Rp =   '),(3,1))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Rp_step =   '),(4,1))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Max_Rp =   '),(5,1))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Tol_x_step =   '),(7,1))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Max_tol_x =   '),(8,1))
#         Gsizer3.Add(wx.StaticText(self.ParameterTab,-1,'   Core_tol =   '),(10,1))
# 
#         self.Ttol_x = wx.TextCtrl( self.ParameterTab, -1, str(self.par[6]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_Ttol_x, self.Ttol_x)
#         Gsizer3.Add(self.Ttol_x,(0,2))
#         
#         self.Ttol_z = wx.TextCtrl( self.ParameterTab, -1, str(self.par[7]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_Ttol_z, self.Ttol_z)
#         Gsizer3.Add(self.Ttol_z,(1,2))
#         
#         self.TRp = wx.TextCtrl( self.ParameterTab, -1, str(self.par[8]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_TRp, self.TRp)
#         Gsizer3.Add(self.TRp,(3,2))
#         
#         self.TRp_step = wx.TextCtrl( self.ParameterTab, -1, str(self.par[9]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_TRp_step, self.TRp_step)
#         Gsizer3.Add(self.TRp_step,(4,2))
#         
#         self.TMax_Rp = wx.TextCtrl( self.ParameterTab, -1, str(self.par[10]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_TMax_Rp, self.TMax_Rp)
#         Gsizer3.Add(self.TMax_Rp,(5,2))
# 
#         self.Ttol_x_step = wx.TextCtrl( self.ParameterTab, -1, str(self.par[11]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_Ttol_x_step, self.Ttol_x_step)
#         Gsizer3.Add(self.Ttol_x_step,(7,2))
# 
#         self.TMax_tol_x = wx.TextCtrl( self.ParameterTab, -1, str(self.par[12]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_TMax_tol_x, self.TMax_tol_x)
#         Gsizer3.Add(self.TMax_tol_x,(8,2))
# 
#         self.Tcore_tol = wx.TextCtrl( self.ParameterTab, -1, str(self.par[13]), size=(70, -1))
#         self.Bind(wx.EVT_TEXT, self.on_Tcore_tol, self.Tcore_tol)
#         Gsizer3.Add(self.Tcore_tol,(10,2))
#         
#         sizer2.Add(Gsizer3, 0, wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
# 
#         btnsizer = wx.StdDialogButtonSizer()
#           
#         if wx.Platform != "__WXMSW__":
#             btn = wx.ContextHelpButton(self)
#             btnsizer.AddButton(btn)
#           
#         btn = wx.Button(self, wx.ID_OK)
#         btn.SetDefault()
#         btnsizer.AddButton(btn)
#   
#         btn = wx.Button(self, wx.ID_CANCEL)
#         btnsizer.AddButton(btn)
#         btnsizer.Realize()
#   
#         sizer.Add(self.nb, 5, flag =  wx.ALL  | wx.ALIGN_CENTER_VERTICAL)
#         sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
#  
#         self.ProblemTab.SetSizer(sizer1)
#         self.ParameterTab.SetSizer(sizer2)
# 
#         self.SetSizer(sizer)
#         sizer.Fit(self)
#         
#         self.update_list()
# 
#     def OnGroup1Select( self, event ):
# #         radio_selected = event.GetEventObject()
#         radio = self.group1_ctrls[0]
#         if radio.GetValue() == True:
#             self.obj_out = 1
# #             self.CheckC1.SetValue(False)
#             self.CheckC2.Enable(True)
#             self.CheckC1.Enable(False)
#             
#             self.textC11.Enable(False)
#             self.textC12.Enable(False)
#             if self.CheckC2.GetValue() == True:       
#                 self.textC21.Enable(True)
#                 self.textC22.Enable(True)     
#         
#         radio = self.group1_ctrls[1]    
#         if radio.GetValue() == True:
#             self.obj_out = 2
# #             self.CheckC2.SetValue(False)
#             self.CheckC2.Enable(False)
#             self.CheckC1.Enable(True)
#            
#             self.textC21.Enable(False)
#             self.textC22.Enable(False)
#             if self.CheckC1.GetValue() == True:
#                 self.textC11.Enable(True)
#                 self.textC12.Enable(True)
#             
#     def on_CheckC1(self,event):
#         if event.IsChecked():
#             self.constr_out[0][0] = 1
#             self.textC11.Enable(True)
#             self.textC12.Enable(True)
#         else:
#             self.constr_out[0][0] = 0
#             self.textC11.Enable(False)
#             self.textC12.Enable(False)
#     
#     def on_CheckC2(self,event):
#         if event.IsChecked():
#             self.constr_out[1][0] = 2
#             self.textC21.Enable(True)
#             self.textC22.Enable(True)
#         else:
#             self.constr_out[1][0] = 0
#             self.textC21.Enable(False)
#             self.textC22.Enable(False)
#     
#     def on_CheckC3(self,event):
#         if event.IsChecked():
#             self.constr_out[2][0] = 3
#             self.textC31.Enable(True)
#             self.textC32.Enable(True)
#         else:
#             self.constr_out[2][0] = 0
#             self.textC31.Enable(False)
#             self.textC32.Enable(False)
#     
#     def on_CheckC4(self,event):
#         if event.IsChecked():
#             self.constr_out[3][0] = 4
#             self.textC41.Enable(True)
#             self.textC42.Enable(True)
#         else:
#             self.constr_out[3][0] = 0
#             self.textC41.Enable(False)
#             self.textC42.Enable(False)
# 
#     def on_CheckOneV(self,event):
#         pass
# 
#     def EvtCombo1(self,event):
#         if self.combo1.GetValue() == self.types[0]:
#             self.Txt11.SetLabel('  Element IDs  ')
#             self.Txt12.SetLabel('  Lower bound  ')
#             self.Txt13.SetLabel('  Upper bound  ')
#             self.Gscrsizer.Layout()
#         elif self.combo1.GetValue() == self.types[1]:
#             self.Txt11.SetLabel('  Node IDs  ')
#             self.Txt12.SetLabel('  dx1  ')
#             self.Txt13.SetLabel('  dx2  ')
# #             self.Txt12.Wrap(160)
# #             self.Txt13.Wrap(160)
#             self.Gscrsizer.Layout()
#         elif self.combo1.GetValue() == self.types[2]:
#             self.Txt11.SetLabel('  Node IDs  ')
#             self.Txt12.SetLabel('  dy1  ')
#             self.Txt13.SetLabel('  dy2  ')
#             self.Gscrsizer.Layout()
# 
#     def GetSelectedItems(self):
#         selection = []
#         index = self.list_ctrl.GetFirstSelected()
#         selection.append(index)
#         while len(selection) != self.list_ctrl.GetSelectedItemCount() and index != -1:
#             index = self.list_ctrl.GetNextSelected(index)
#             selection.append(index)
#             
#         return selection
# 
#     def update_list(self):
#         self.list_ctrl.DeleteAllItems()
#         for i in range(0,len(self.vars)):
#             self.list_ctrl.InsertStringItem(i, str(self.vars[i][0]))
#             self.list_ctrl.SetStringItem(i, 1, str(self.types[int(self.vars[i][1])]))
#             self.list_ctrl.SetStringItem(i, 2, str(self.vars[i][2]))
#             self.list_ctrl.SetStringItem(i, 3, str(self.vars[i][3]))
#             self.list_ctrl.SetStringItem(i, 4, str(self.vars[i][4]))           
# 
#     def renumber_list(self):
#         indct = np.zeros(len(self.vars))
#         for i in range(0,len(self.vars)-1):
#             if self.vars[i][0] == self.vars[i+1][0]:
#                 indct[i] = 1
#         if self.vars[-1][0] == self.vars[-2][0]:
#             indct[-1] = 1
#         
#         k = 1
#         for i in range(0,len(self.vars)):
#             self.vars[i][0] = k
#             if indct[i] == 0:
#                 k = k+1
#         
#         self.Nvar = 0        
#         for i in range(0,len(self.vars)):
#             if self.Nvar < self.vars[i][0]:
#                 self.Nvar = self.vars[i][0]
#                 
#         print self.Nvar
# 
#     def delete_variable(self,event):
#         IDs = self.GetSelectedItems()
#         if IDs[0] != -1:
#             for i in reversed(IDs):
#                 del self.vars[i]
#                 self.list_index = self.list_index - 1
# 
#         self.renumber_list()
#         self.update_list()
#         
#     def add_variable(self,event):
#         if self.textT0.GetValue() == '':
#             return
#         IDs = self.textT0.GetValue().split(",")
#         if self.CheckOneV.GetValue() == True:
#             self.Nvar = self.Nvar + 1
#         for i in range(0,len(IDs)):
#             self.list_ctrl.InsertStringItem(self.list_index, str(self.Nvar))
#             if self.CheckOneV.GetValue() == False:
#                 self.Nvar = self.Nvar + 1
#             self.list_ctrl.SetStringItem(self.list_index, 1, self.combo1.GetValue())   
#             self.list_ctrl.SetStringItem(self.list_index, 2, IDs[i])
#             self.list_ctrl.SetStringItem(self.list_index, 3, self.textT1.GetValue())
#             self.list_ctrl.SetStringItem(self.list_index, 4, self.textT2.GetValue())
#             if self.combo1.GetValue() == self.types[0]:
#                 typ = 0
#             elif self.combo1.GetValue() == self.types[1]:
#                 typ = 1
#             elif self.combo1.GetValue() == self.types[2]:
#                 typ = 2
#             self.vars.append([self.Nvar,typ,int(IDs[i]),float(self.textT1.GetValue()),float(self.textT2.GetValue())])
#             self.list_index = self.list_index + 1     
#         self.textT0.SetValue('')
#         self.textT1.SetValue('0.0')
#         self.textT2.SetValue('0.0')
#         self.renumber_list()
#         self.update_list()
# 
#     def on_Tnchain(self,event):
#         self.par_out[0] = int(event.GetString())
#     
#     def on_Tsapo(self,event):
#         self.par_out[1] = int(event.GetString())
#     
#     def on_Tmaxsim(self,event):
#         self.par_out[2] = int(event.GetString())
#     
#     def on_Tsisusp1(self,event):
#         self.par_out[3] = float(event.GetString())
# 
#     def on_Tsisusp2(self,event):
#         self.par_out[4] = float(event.GetString())
# 
#     def EvtCombo2(self,event):
#         if self.combo2.GetValue() == self.Shape_subsp[0]:
#             self.par_out[5] = 1
#         elif self.combo2.GetValue() == self.Shape_subsp[1]:
#             self.par_out[5] = 2
#         elif self.combo2.GetValue() == self.Shape_subsp[2]:
#             self.par_out[5] = 3
# 
#     def on_Ttol_x(self,event):
#         self.par_out[6] = float(event.GetString())
#         
#     def on_Ttol_z(self,event):
#         self.par_out[7] = float(event.GetString())
# 
#     def on_TRp(self,event):
#         self.par_out[8] = float(event.GetString())
# 
#     def on_TRp_step(self,event):
#         self.par_out[9] = float(event.GetString())
#         
#     def on_TMax_Rp(self,event):
#         self.par_out[10] = float(event.GetString())
# 
#     def on_Ttol_x_step(self,event):
#         self.par_out[11] = float(event.GetString())
#         
#     def on_TMax_tol_x(self,event):
#         self.par_out[12] = float(event.GetString())
# 
#     def on_Tcore_tol(self,event):
#         self.par_out[13] = float(event.GetString())       
#         
#     def EvtTextC11(self,event):
#         self.constr_out[0][1] = float(event.GetString())
# 
#     def EvtTextC12(self,event):
#         self.constr_out[0][2] = float(event.GetString())
#         
#     def EvtTextC21(self,event):
#         self.constr_out[1][1] = float(event.GetString())
#         
#     def EvtTextC22(self,event):
#         self.constr_out[1][2] = float(event.GetString())
# 
#     def EvtTextC31(self,event):
#         self.constr_out[2][1] = float(event.GetString())
#         
#     def EvtTextC32(self,event):
#         self.constr_out[2][2] = float(event.GetString())
#         
#     def EvtTextC41(self,event):
#         self.constr_out[3][1] = float(event.GetString())
#         
#     def EvtTextC42(self,event):
#         self.constr_out[3][2] = float(event.GetString())