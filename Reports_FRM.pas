unit Reports_FRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, FMX.StdCtrls,
  FMX.DateTimeCtrls, Data.DB, Data.Win.ADODB;

type
  TReports = class(TForm)
    Z: TRectangle;
    Rectangle18: TRectangle;
    DateEdit2: TDateEdit;
    DateEdit1: TDateEdit;
    Rectangle17: TRectangle;
    labels: TLabel;
    rExportTask: TRectangle;
    Label4: TLabel;
    rExportBay: TRectangle;
    Label1: TLabel;
    rExportConflict: TRectangle;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    StyleBook1: TStyleBook;
    rLogoImage: TRectangle;
    rLogo: TRectangle;
    rDashBoardMenubtn: TRectangle;
    DashBoardbtnICON: TRectangle;
    lblDashBoardBtn: TEdit;
    rTaskManagerMenuBtn: TRectangle;
    TaskManagerIcon: TRectangle;
    lblTaskManagerBtn: TEdit;
    rBayManagerMenuBtn: TRectangle;
    BayManagerIcon: TRectangle;
    lblBayManagerbtn: TEdit;
    rSchedulerMenuBtn: TRectangle;
    SchedulerIcon: TRectangle;
    lblSchedulerBtn: TEdit;
    rReportsMenuBtn: TRectangle;
    ReportsIcon: TRectangle;
    lblReportsbtn: TEdit;
    rStaffManagerMenubtn: TRectangle;
    StaffManagerIcon: TRectangle;
    lblStaffManagerBtn: TEdit;
    rStaffSchedulerMenuBtn: TRectangle;
    staffSchedulerICON: TRectangle;
    lblStaffSchedulerbtn: TEdit;
    LogoutIcon: TRectangle;
    LogoutBtn: TEdit;
    procedure FormShow(Sender: TObject);
    procedure rExportTaskClick(Sender: TObject);
    procedure rExportBayClick(Sender: TObject);
    procedure rExportConflictClick(Sender: TObject);
  private
    { Private declarations }
    function GetSanitizedFileName(const ABaseName: string): string;
  public
    { Public declarations }
  end;

var
  Reports: TReports;

implementation

{$R *.fmx}

uses dmOpti, Login_FRM, System.DateUtils;

procedure TReports.FormShow(Sender: TObject);
begin
  // Set the default date range to the current day when the form is shown
  DateEdit1.Date := Trunc(Now);
  DateEdit2.Date := Trunc(Now);
end;

function TReports.GetSanitizedFileName(const ABaseName: string): string;
begin
  // Helper function to create a clean filename with the current date, e.g., "Task_Log_Report_2025-07-31.csv"
  Result := ABaseName + '_' + FormatDateTime('yyyy-mm-dd', Now) + '.csv';
end;

procedure TReports.rExportTaskClick(Sender: TObject);
var
  ReportData: TStringList;
  qry: TADOQuery;
  lStartDate, lEndDate: TDateTime;
begin
  lStartDate := Trunc(DateEdit1.Date);
  lEndDate := Trunc(DateEdit2.Date) + 1; // Add 1 to include the entire end day in the query

  ReportData := TStringList.Create;
  qry := TADOQuery.Create(nil);
  try
    // --- 1. Build the Report Header ---
    ReportData.Add('OptiFlow - Detailed Task Log');
    ReportData.Add('Date Range,' + FormatDateTime('yyyy-mm-dd', lStartDate) + ' to ' + FormatDateTime('yyyy-mm-dd', Trunc(lEndDate-1)));
    ReportData.Add(''); // Blank line for spacing
    ReportData.Add('Task Name,Bay Name,Staff Member,Start Time,End Time,Duration (min),Priority,Status');

    // --- 2. Fetch Detailed Task Data from the database ---
    qry.Connection := FmOpti.ConOpti;
    qry.SQL.Text := 'SELECT t.TaskName, b.BayName, st.Username, s.StartTime, t.Duration, t.Priority, t.Status ' +
                    'FROM (((tblSchedule s ' +
                    'LEFT JOIN tblTasks t ON s.TaskID = t.TaskID) ' +
                    'LEFT JOIN tblBays b ON s.BayID = b.BayID) ' +
                    'LEFT JOIN tblStaff st ON s.StaffID = st.StaffID) ' +
                    'WHERE t.ManagerID = :pManagerID AND s.StartTime >= :pStartDate AND s.StartTime < :pEndDate ' +
                    'ORDER BY s.StartTime';
    qry.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    qry.Parameters.ParamByName('pStartDate').Value := lStartDate;
    qry.Parameters.ParamByName('pEndDate').Value := lEndDate;
    qry.Open;

    if qry.IsEmpty then
    begin
      ReportData.Add('No tasks scheduled for this period.');
    end
    else
    begin
      while not qry.Eof do
      begin
        // Format each row as a comma-separated line for the CSV file
        ReportData.Add(Format('"%s","%s","%s",%s,%s,%d,%s,%s',
          [qry.FieldByName('TaskName').AsString,
           qry.FieldByName('BayName').AsString,
           qry.FieldByName('Username').AsString,
           FormatDateTime('yyyy-mm-dd hh:nn', qry.FieldByName('StartTime').AsDateTime),
           FormatDateTime('yyyy-mm-dd hh:nn', IncMinute(qry.FieldByName('StartTime').AsDateTime, qry.FieldByName('Duration').AsInteger)),
           qry.FieldByName('Duration').AsInteger,
           qry.FieldByName('Priority').AsString,
           qry.FieldByName('Status').AsString]));
        qry.Next;
      end;
    end;

    // --- 3. Prompt the user to save the report file ---
    SaveDialog1.Title := 'Save Task Log Report';
    SaveDialog1.Filter := 'CSV File (*.csv)|*.csv';
    SaveDialog1.FileName := GetSanitizedFileName('Task_Log_Report');

    if SaveDialog1.Execute then
    begin
      ReportData.SaveToFile(SaveDialog1.FileName);
      ShowMessage('Report saved successfully to ' + SaveDialog1.FileName);
    end;

  finally
    ReportData.Free;
    qry.Free;
  end;
end;

procedure TReports.rExportBayClick(Sender: TObject);
var
  ReportData: TStringList;
  qry: TADOQuery;
  lStartDate, lEndDate: TDateTime;
begin
  lStartDate := Trunc(DateEdit1.Date);
  lEndDate := Trunc(DateEdit2.Date) + 1;

  ReportData := TStringList.Create;
  qry := TADOQuery.Create(nil);
  try
    ReportData.Add('OptiFlow - Bay Usage Report');
    ReportData.Add('Date Range,' + FormatDateTime('yyyy-mm-dd', lStartDate) + ' to ' + FormatDateTime('yyyy-mm-dd', Trunc(lEndDate-1)));
    ReportData.Add('');
    ReportData.Add('Bay Name,Total Tasks Assigned,Total Minutes Used');

    qry.Connection := FmOpti.ConOpti;
    // This query groups by Bay Name and calculates the total tasks and duration for each bay
    qry.SQL.Text := 'SELECT b.BayName, COUNT(s.TaskID) AS TaskCount, SUM(t.Duration) AS TotalDuration ' +
                    'FROM (tblSchedule s INNER JOIN tblTasks t ON s.TaskID = t.TaskID) ' +
                    'INNER JOIN tblBays b ON s.BayID = b.BayID ' +
                    'WHERE t.ManagerID = :pManagerID AND s.StartTime >= :pStartDate AND s.StartTime < :pEndDate ' +
                    'GROUP BY b.BayName ORDER BY b.BayName';
    qry.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    qry.Parameters.ParamByName('pStartDate').Value := lStartDate;
    qry.Parameters.ParamByName('pEndDate').Value := lEndDate;
    qry.Open;

    if qry.IsEmpty then
    begin
      ReportData.Add('No bay usage data for this period.');
    end
    else
    begin
      while not qry.Eof do
      begin
        ReportData.Add(Format('"%s",%d,%d',
          [qry.FieldByName('BayName').AsString,
           qry.FieldByName('TaskCount').AsInteger,
           qry.FieldByName('TotalDuration').AsInteger]));
        qry.Next;
      end;
    end;

    SaveDialog1.Title := 'Save Bay Usage Report';
    SaveDialog1.Filter := 'CSV File (*.csv)|*.csv';
    SaveDialog1.FileName := GetSanitizedFileName('Bay_Usage_Report');

    if SaveDialog1.Execute then
    begin
      ReportData.SaveToFile(SaveDialog1.FileName);
      ShowMessage('Report saved successfully to ' + SaveDialog1.FileName);
    end;

  finally
    ReportData.Free;
    qry.Free;
  end;
end;

procedure TReports.rExportConflictClick(Sender: TObject);
var
  ReportData: TStringList;
  qry: TADOQuery;
  lStartDate, lEndDate: TDateTime;
begin
  lStartDate := Trunc(DateEdit1.Date);
  lEndDate := Trunc(DateEdit2.Date) + 1;

  ReportData := TStringList.Create;
  qry := TADOQuery.Create(nil);
  try
    ReportData.Add('OptiFlow - High-Priority Task Report');
    ReportData.Add('Date Range,' + FormatDateTime('yyyy-mm-dd', lStartDate) + ' to ' + FormatDateTime('yyyy-mm-dd', Trunc(lEndDate-1)));
    ReportData.Add('');
    ReportData.Add('Task Name,Bay Name,Staff Member,Start Time,Priority,Status');

    qry.Connection := FmOpti.ConOpti;
    // This query is similar to the task log but filters specifically for tasks where Priority is 'High'
    qry.SQL.Text := 'SELECT t.TaskName, b.BayName, st.Username, s.StartTime, t.Priority, t.Status ' +
                    'FROM (((tblSchedule s ' +
                    'LEFT JOIN tblTasks t ON s.TaskID = t.TaskID) ' +
                    'LEFT JOIN tblBays b ON s.BayID = b.BayID) ' +
                    'LEFT JOIN tblStaff st ON s.StaffID = st.StaffID) ' +
                    'WHERE t.ManagerID = :pManagerID AND t.Priority = "High" AND s.StartTime >= :pStartDate AND s.StartTime < :pEndDate ' +
                    'ORDER BY s.StartTime';
    qry.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
    qry.Parameters.ParamByName('pStartDate').Value := lStartDate;
    qry.Parameters.ParamByName('pEndDate').Value := lEndDate;
    qry.Open;

    if qry.IsEmpty then
    begin
      ReportData.Add('No high-priority tasks scheduled for this period.');
    end
    else
    begin
      while not qry.Eof do
      begin
        ReportData.Add(Format('"%s","%s","%s",%s,%s,%s',
          [qry.FieldByName('TaskName').AsString,
           qry.FieldByName('BayName').AsString,
           qry.FieldByName('Username').AsString,
           FormatDateTime('yyyy-mm-dd hh:nn', qry.FieldByName('StartTime').AsDateTime),
           qry.FieldByName('Priority').AsString,
           qry.FieldByName('Status').AsString]));
        qry.Next;
      end;
    end;

    SaveDialog1.Title := 'Save High-Priority Task Report';
    SaveDialog1.Filter := 'CSV File (*.csv)|*.csv';
    SaveDialog1.FileName := GetSanitizedFileName('High_Priority_Report');

    if SaveDialog1.Execute then
    begin
      ReportData.SaveToFile(SaveDialog1.FileName);
      ShowMessage('Report saved successfully to ' + SaveDialog1.FileName);
    end;

  finally
    ReportData.Free;
    qry.Free;
  end;
end;

end.

