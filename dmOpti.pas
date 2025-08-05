unit dmOpti;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Data.Win.ADODB, System.Generics.Collections,
  System.DateUtils; // Added for IncMinute

type
  // This record is used by the new OptimizeSchedule function
  TBayInfo = record
    BayID: Integer;
    BayType: string;
    NextAvailableTime: TDateTime;
  end;

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
    function GetEarliestStartTimeForBay(const ABayID: Integer): TDateTime;
  public
    { Public declarations }
    // *** NEW PUBLIC FUNCTION ***
    function OptimizeSchedule(AManagerID: Integer): Boolean;
  end;

var
  FmOpti: TFmOpti;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

// *** NEW HELPER FUNCTION (Moved from Scheduler Form) ***
function TFmOpti.GetEarliestStartTimeForBay(const ABayID: Integer): TDateTime;
var
  qryLastEnd: TADOQuery;
begin
  Result := Now;
  qryLastEnd := TADOQuery.Create(nil);
  try
    qryLastEnd.Connection := ConOpti;
    qryLastEnd.SQL.Text := 'SELECT MAX(EndTime) AS LastEnd FROM tblSchedule WHERE BayID = :pBayID';
    qryLastEnd.Parameters.ParamByName('pBayID').Value := ABayID;
    qryLastEnd.Open;
    if not qryLastEnd.FieldByName('LastEnd').IsNull then
    begin
      Result := qryLastEnd.FieldByName('LastEnd').AsDateTime;
    end;
  finally
    qryLastEnd.Free;
  end;
end;

// *** NEW IMPLEMENTATION OF THE SCHEDULING ALGORITHM ***
function TFmOpti.OptimizeSchedule(AManagerID: Integer): Boolean;
var
  qryUnscheduled, qryAvailableBays, cmd: TADOQuery;
  lAvailableBays: TList<TBayInfo>;
  lBayInfo: TBayInfo;
  lTaskID, lDuration, i: Integer;
  lRequiredBayType: string;
  lBestBayID, lBestBayIndex: Integer;
  lBestStartTime, lEndTime: TDateTime;
begin
  Result := False; // Assume nothing is scheduled initially
  lAvailableBays := TList<TBayInfo>.Create;
  qryUnscheduled := TADOQuery.Create(nil);
  qryAvailableBays := TADOQuery.Create(nil);
  cmd := TADOQuery.Create(nil);
  try
    qryUnscheduled.Connection := ConOpti;
    qryAvailableBays.Connection := ConOpti;
    cmd.Connection := ConOpti;

    // Get all available bays and their next free time
    qryAvailableBays.SQL.Text := 'SELECT BayID, BayType FROM tblBays WHERE Status = ''Available'' AND ManagerID = :pManagerID';
    qryAvailableBays.Parameters.ParamByName('pManagerID').Value := AManagerID;
    qryAvailableBays.Open;
    while not qryAvailableBays.Eof do
    begin
      lBayInfo.BayID := qryAvailableBays.FieldByName('BayID').AsInteger;
      lBayInfo.BayType := qryAvailableBays.FieldByName('BayType').AsString;
      lBayInfo.NextAvailableTime := GetEarliestStartTimeForBay(lBayInfo.BayID);
      lAvailableBays.Add(lBayInfo);
      qryAvailableBays.Next;
    end;
    qryAvailableBays.Close;

    if lAvailableBays.Count = 0 then
      Exit; // Exit if no bays are available

    ConOpti.BeginTrans;
    try
      // Get all unscheduled tasks, ordered by priority
      qryUnscheduled.SQL.Text := 'SELECT TaskID, Duration, BayType, Priority FROM tblTasks ' +
                               'WHERE (Status IS NULL OR Status = ''Pending'') AND ManagerID = :pManagerID ' +
                               'ORDER BY IIF(Priority="High", 1, IIF(Priority="Medium", 2, 3))';
      qryUnscheduled.Parameters.ParamByName('pManagerID').Value := AManagerID;
      qryUnscheduled.Open;

      while not qryUnscheduled.Eof do
      begin
        lTaskID := qryUnscheduled.FieldByName('TaskID').AsInteger;
        lDuration := qryUnscheduled.FieldByName('Duration').AsInteger;
        lRequiredBayType := qryUnscheduled.FieldByName('BayType').AsString;
        lBestBayID := -1;
        lBestBayIndex := -1;
        lBestStartTime := EncodeDate(2099, 1, 1);

        // Find the best bay for the current task
        for i := 0 to lAvailableBays.Count - 1 do
        begin
          lBayInfo := lAvailableBays[i];
          if (lBayInfo.BayType = lRequiredBayType) and (lBayInfo.NextAvailableTime < lBestStartTime) then
          begin
            lBestStartTime := lBayInfo.NextAvailableTime;
            lBestBayID := lBayInfo.BayID;
            lBestBayIndex := i;
          end;
        end;

        // If a suitable bay was found, schedule the task
        if lBestBayID <> -1 then
        begin
          lEndTime := IncMinute(lBestStartTime, lDuration);
          cmd.SQL.Text := 'INSERT INTO tblSchedule (TaskID, BayID, StartTime, EndTime, ManagerID) VALUES (:pTaskID, :pBayID, :pStartTime, :pEndTime, :pManagerID)';
          cmd.Parameters.ParamByName('pTaskID').Value := lTaskID;
          cmd.Parameters.ParamByName('pBayID').Value := lBestBayID;
          cmd.Parameters.ParamByName('pStartTime').Value := lBestStartTime;
          cmd.Parameters.ParamByName('pEndTime').Value := lEndTime;
          cmd.Parameters.ParamByName('pManagerID').Value := AManagerID;
          cmd.ExecSQL;

          cmd.SQL.Text := 'UPDATE tblTasks SET Status = ''Scheduled'' WHERE TaskID = :pTaskID';
          cmd.Parameters.ParamByName('pTaskID').Value := lTaskID;
          cmd.ExecSQL;

          lBayInfo := lAvailableBays[lBestBayIndex];
          lBayInfo.NextAvailableTime := lEndTime;
          lAvailableBays[lBestBayIndex] := lBayInfo;

          Result := True; // Mark that at least one task was scheduled
        end;
        qryUnscheduled.Next;
      end;
      ConOpti.CommitTrans;
    except
      on E: Exception do
      begin
        ConOpti.RollbackTrans;
        raise; // Re-raise the exception to be handled by the form
      end;
    end;
  finally
    lAvailableBays.Free;
    qryUnscheduled.Free;
    qryAvailableBays.Free;
    cmd.Free;
  end;
end;

end.

