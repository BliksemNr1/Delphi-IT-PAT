unit dmOpti;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Data.Win.ADODB;

type
  TFmOpti = class(TDataModule)
    ConOpti: TADOConnection;
    optiBays: TADOTable;
    optiManagers: TADOTable;
    optiTasks: TADOTable;
    optiStaff: TADOTable;
    optiSchedule: TADOTable;
    qryBays: TADOQuery;
    qryManagers: TADOQuery;
    qryTasks: TADOQuery;
    qryStaff: TADOQuery;
    qrySchedule: TADOQuery;
    dsBays: TDataSource;
    dsManagers: TDataSource;
    dsTasks: TDataSource;
    dsStaff: TDataSource;
    dsSchedule: TDataSource;
    dsqryBays: TDataSource;
    dsqryManagers: TDataSource;
    dsqryTasks: TDataSource;
    dsqryStaff: TDataSource;
    dsqrySchedule: TDataSource;
    qryNotifications: TADOQuery;
    dsqryNotifications: TDataSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmOpti: TFmOpti;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
