object FmOpti: TFmOpti
  Height = 600
  Width = 800
  object ConOpti: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=D:\#PAT files\IT PA' +
      'T\OptiFlowTables.mdb;Mode=ReadWrite;Persist Security Info=False;' +
      'Jet OLEDB:System database="";Jet OLEDB:Registry Path="";Jet OLED' +
      'B:Database Password="";Jet OLEDB:Engine Type=5;Jet OLEDB:Databas' +
      'e Locking Mode=1;Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:G' +
      'lobal Bulk Transactions=1;Jet OLEDB:New Database Password="";Jet' +
      ' OLEDB:Create System Database=False;Jet OLEDB:Encrypt Database=F' +
      'alse;Jet OLEDB:Don'#39't Copy Locale on Compact=False;Jet OLEDB:Comp' +
      'act Without Replica Repair=False;Jet OLEDB:SFP=False'
    LoginPrompt = False
    Mode = cmReadWrite
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 83
    Top = 96
  end
  object optiBays: TADOTable
    Active = True
    Connection = ConOpti
    CursorType = ctStatic
    TableName = 'tblBays'
    Left = 83
    Top = 192
  end
  object optiManagers: TADOTable
    Active = True
    Connection = ConOpti
    CursorType = ctStatic
    TableName = 'tblManagers'
    Left = 83
    Top = 269
  end
  object optiTasks: TADOTable
    Active = True
    Connection = ConOpti
    CursorType = ctStatic
    TableName = 'tblTasks'
    Left = 83
    Top = 352
  end
  object optiStaff: TADOTable
    Active = True
    Connection = ConOpti
    CursorType = ctStatic
    TableName = 'tblStaff'
    Left = 83
    Top = 435
  end
  object optiSchedule: TADOTable
    Active = True
    Connection = ConOpti
    CursorType = ctStatic
    TableName = 'tblSchedule'
    Left = 83
    Top = 512
  end
  object qryBays: TADOQuery
    Connection = ConOpti
    DataSource = dsqryBays
    Parameters = <>
    Left = 566
    Top = 200
  end
  object qryManagers: TADOQuery
    Connection = ConOpti
    DataSource = dsqryManagers
    Parameters = <>
    Left = 566
    Top = 277
  end
  object qryTasks: TADOQuery
    Connection = ConOpti
    DataSource = dsqryTasks
    Parameters = <>
    Left = 566
    Top = 344
  end
  object qryStaff: TADOQuery
    Connection = ConOpti
    DataSource = dsqryStaff
    Parameters = <>
    Left = 566
    Top = 411
  end
  object qrySchedule: TADOQuery
    Connection = ConOpti
    DataSource = dsqrySchedule
    Parameters = <>
    Left = 566
    Top = 480
  end
  object dsBays: TDataSource
    DataSet = optiBays
    Left = 384
    Top = 192
  end
  object dsManagers: TDataSource
    DataSet = optiManagers
    Left = 384
    Top = 269
  end
  object dsTasks: TDataSource
    DataSet = optiTasks
    Left = 384
    Top = 352
  end
  object dsStaff: TDataSource
    DataSet = optiStaff
    Left = 384
    Top = 435
  end
  object dsSchedule: TDataSource
    DataSet = optiSchedule
    Left = 384
    Top = 512
  end
  object dsqryBays: TDataSource
    DataSet = optiBays
    Left = 656
    Top = 216
  end
  object dsqryManagers: TDataSource
    DataSet = optiManagers
    Left = 656
    Top = 277
  end
  object dsqryTasks: TDataSource
    DataSet = optiTasks
    Left = 656
    Top = 344
  end
  object dsqryStaff: TDataSource
    DataSet = optiStaff
    Left = 656
    Top = 411
  end
  object dsqrySchedule: TDataSource
    DataSet = optiSchedule
    Left = 656
    Top = 480
  end
  object optiNotifications: TADOTable
    Connection = ConOpti
    CursorType = ctStatic
    TableName = 'tblNotifications'
    Left = 83
    Top = 32
  end
  object dsNotifications: TDataSource
    DataSet = optiNotifications
    Left = 384
    Top = 32
  end
  object qryNotifications: TADOQuery
    Connection = ConOpti
    Parameters = <>
    Left = 566
    Top = 32
  end
  object dsqryNotifications: TDataSource
    DataSet = qryNotifications
    Left = 656
    Top = 32
  end
end