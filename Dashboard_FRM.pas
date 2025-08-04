unit Dashboard_FRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.Edit, FMX.DateTimeCtrls, FMX.StdCtrls,
  FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart, System.UIConsts,
  Data.DB, Data.Win.ADODB, FMX.Ani;

type
  TfDashboard = class(TForm)
    rDateRangeBackground: TRectangle;
    Rectangle18: TRectangle;
    DateEdit1: TDateEdit;
    DateEdit2: TDateEdit;
    Rectangle17: TRectangle;
    StyleBook1: TStyleBook;
    labels: TLabel;
    rTaskInDateRangbackground: TRectangle;
    rAverageTaskDurationBackground: TRectangle;
    lblTasksInDateRange: TLabel;
    LblTasks: TLabel;
    lblAverAgeTaskDuration: TLabel;
    lblMins: TLabel;
    rTaskChartbackground: TRectangle;
    lblTaskChart: TLabel;
    rUtilizationChartBackground: TRectangle;
    lblUtilizationChart: TLabel;
    rConflictsBackground: TRectangle;
    LlblConflictsChart: TLabel;
    rExport: TRectangle;
    Label5: TLabel;
    TaskChart: TChart;
    Series1: TBarSeries;
    ConflictsChart: TChart;
    BarSeries1: TBarSeries;
    UtilizationChart: TChart;
    Series2: TPieSeries;
    lblutilizationperc: TLabel;
    lblMinsText: TLabel;
    SaveDialog1: TSaveDialog;
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
    aniLoading: TAniIndicator;
    procedure FormCreate(Sender: TObject);
    procedure DateChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rExportClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FSpinnerTimer: TTimer;
    procedure UpdateDashboardWithLiveData;
    procedure StyleAllCharts;
    procedure AnimatePulse(LabelObj: TLabel);
    procedure AnimationFinish(Sender: TObject);
    procedure GlowRectangle(Rect: TRectangle);
    procedure StartLoading;
    procedure StopLoading;
    procedure CenterOverlayControls;
    procedure SpinnerTimerHandler(Sender: TObject);
  public
  end;

var
  fDashboard: TfDashboard;

implementation

{$R *.fmx}

uses dmOpti, Login_FRM;

procedure TfDashboard.FormCreate(Sender: TObject);
begin
  StyleAllCharts;
  Application.ShowHint := True;
  lblTaskChart.Hint := 'Bar chart showing all scheduled tasks for the selected date range.';
  lblTaskChart.ShowHint := True;
  lblUtilizationChart.Hint := 'Pie chart showing bay utilization percentage for the selected range.';
  lblUtilizationChart.ShowHint := True;
  lblAverAgeTaskDuration.Hint := 'Average task duration in minutes.';
  lblAverAgeTaskDuration.ShowHint := True;
  lblTasksInDateRange.Hint := 'Total number of tasks scheduled in the selected period.';
  lblTasksInDateRange.ShowHint := True;

  with aniLoading do
  begin
    Width := 40;
    Height := 40;
    Enabled := False;
    Visible := False;
    Align := TAlignLayout.None;
    BringToFront;
  end;

  FSpinnerTimer := TTimer.Create(Self);
  FSpinnerTimer.Interval := 200;
  FSpinnerTimer.Enabled := False;
  FSpinnerTimer.OnTimer := SpinnerTimerHandler;

  CenterOverlayControls;
end;

procedure TfDashboard.FormShow(Sender: TObject);
begin
  DateEdit1.Date := Trunc(Now);
  DateEdit2.Date := Trunc(Now);
  DateEdit1.OnChange := DateChanged;
  DateEdit2.OnChange := DateChanged;
  UpdateDashboardWithLiveData;
end;

procedure TfDashboard.FormResize(Sender: TObject);
begin
  CenterOverlayControls;
end;

procedure TfDashboard.CenterOverlayControls;
begin
  if Assigned(aniLoading) then
  begin
    aniLoading.Position.X := (Self.Width - aniLoading.Width) / 2;
    aniLoading.Position.Y := (Self.Height - aniLoading.Height) / 2;
  end;
end;

procedure TfDashboard.DateChanged(Sender: TObject);
begin
  UpdateDashboardWithLiveData;
end;

procedure TfDashboard.StartLoading;
begin
  CenterOverlayControls;
  aniLoading.Opacity := 0;
  aniLoading.Visible := True;
  aniLoading.Enabled := True;
  TAnimator.AnimateFloat(aniLoading, 'Opacity', 1, 0.2);
  aniLoading.BringToFront;
  Application.ProcessMessages;
end;

procedure TfDashboard.StopLoading;
begin
  TAnimator.AnimateFloat(aniLoading, 'Opacity', 0, 0.2);
  aniLoading.Enabled := False;
  FSpinnerTimer.Enabled := True;
end;

procedure TfDashboard.SpinnerTimerHandler(Sender: TObject);
begin
  aniLoading.Visible := False;
  FSpinnerTimer.Enabled := False;
end;

procedure TfDashboard.rExportClick(Sender: TObject);
var
  ReportData: TStringList;
  qry: TADOQuery;
  lStartDate, lEndDate: TDateTime;
  lFileName: string;
begin
  StartLoading;
  try
    lStartDate := Trunc(DateEdit1.Date);
    lEndDate := Trunc(DateEdit2.Date) + 1;

    ReportData := TStringList.Create;
    qry := TADOQuery.Create(nil);
    try
      ReportData.Add('OptiFlow Dashboard Report');
      ReportData.Add('Date Range,' + FormatDateTime('yyyy-mm-dd', lStartDate) + ' to ' + FormatDateTime('yyyy-mm-dd', Trunc(lEndDate-1)));
      ReportData.Add('');
      ReportData.Add('KPI,Value');
      ReportData.Add('Total Scheduled Tasks,' + LblTasks.Text);
      ReportData.Add('Average Task Duration (min),' + lblMins.Text);
      ReportData.Add('Bay Utilization,' + lblutilizationperc.Text);
      ReportData.Add('');
      ReportData.Add('--- Detailed Task Log ---');
      ReportData.Add('Task Name,Bay Name,Start Time,Duration,Priority,Status');

      qry.Connection := FmOpti.ConOpti;
      qry.SQL.Text := 'SELECT t.TaskName, b.BayName, s.StartTime, t.Duration, t.Priority, t.Status ' +
                      'FROM (tblTasks t INNER JOIN tblSchedule s ON t.TaskID = s.TaskID) ' +
                      'INNER JOIN tblBays b ON s.BayID = b.BayID ' +
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
          ReportData.Add(Format('"%s",%s,"%s",%d,%s,%s',
            [qry.FieldByName('TaskName').AsString,
             qry.FieldByName('BayName').AsString,
             FormatDateTime('yyyy-mm-dd hh:nn', qry.FieldByName('StartTime').AsDateTime),
             qry.FieldByName('Duration').AsInteger,
             qry.FieldByName('Priority').AsString,
             qry.FieldByName('Status').AsString]));
          qry.Next;
        end;
      end;
      qry.Close;

      SaveDialog1.Title := 'Save Dashboard Report';
      SaveDialog1.Filter := 'CSV File (*.csv)|*.csv';
      lFileName := 'OptiFlow_Report_' + FormatDateTime('yyyy-mm-dd', Now) + '.csv';
      SaveDialog1.FileName := lFileName;

      if SaveDialog1.Execute then
      begin
        ReportData.SaveToFile(SaveDialog1.FileName);
        ShowMessage('Report saved successfully to ' + SaveDialog1.FileName);
        GlowRectangle(rExport);
      end;
    finally
      ReportData.Free;
      qry.Free;
    end;
  finally
    StopLoading;
  end;
end;

procedure TfDashboard.UpdateDashboardWithLiveData;
var
  qry: TADOQuery;
  lTotalTasks, lAvgDuration, lUtilization: Integer;
  lStartDate, lEndDate, lCurrentDate: TDateTime;
  lDailyTaskCounts, lDailyConflictCounts: TStringList;
begin
  StartLoading;
  try
    lStartDate := Trunc(DateEdit1.Date);
    lEndDate := Trunc(DateEdit2.Date) + 1;

    qry := TADOQuery.Create(nil);
    lDailyTaskCounts := TStringList.Create;
    lDailyConflictCounts := TStringList.Create;
    try
      qry.Connection := FmOpti.ConOpti;

      qry.SQL.Text := 'SELECT COUNT(t.TaskID) FROM tblTasks t INNER JOIN tblSchedule s ON t.TaskID = s.TaskID ' +
                      'WHERE t.ManagerID = :pManagerID AND s.StartTime >= :pStartDate AND s.StartTime < :pEndDate';
      qry.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
      qry.Parameters.ParamByName('pStartDate').Value := lStartDate;
      qry.Parameters.ParamByName('pEndDate').Value := lEndDate;
      qry.Open;
      lTotalTasks := IfThen(not qry.Eof, qry.Fields[0].AsInteger, 0);
      qry.Close;
      LblTasks.Text := IntToStr(lTotalTasks);
      AnimatePulse(LblTasks);

      qry.SQL.Text := 'SELECT AVG(t.Duration) FROM tblTasks t INNER JOIN tblSchedule s ON t.TaskID = s.TaskID ' +
                      'WHERE t.ManagerID = :pManagerID AND s.StartTime >= :pStartDate AND s.StartTime < :pEndDate';
      qry.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
      qry.Parameters.ParamByName('pStartDate').Value := lStartDate;
      qry.Parameters.ParamByName('pEndDate').Value := lEndDate;
      qry.Open;
      lAvgDuration := IfThen(not qry.Eof, Round(qry.Fields[0].AsFloat), 0);
      qry.Close;
      lblMins.Text := IntToStr(lAvgDuration);
      AnimatePulse(lblMins);

      if lTotalTasks > 0 then
      begin
        qry.SQL.Text := 'SELECT COUNT(t.TaskID) FROM tblTasks t INNER JOIN tblSchedule s ON t.TaskID = s.TaskID ' +
                         'WHERE t.ManagerID = :pManagerID AND t.Status <> ''Pending'' AND s.StartTime >= :pStartDate AND s.StartTime < :pEndDate';
        qry.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
        qry.Parameters.ParamByName('pStartDate').Value := lStartDate;
        qry.Parameters.ParamByName('pEndDate').Value := lEndDate;
        qry.Open;
        lUtilization := IfThen(not qry.Eof, Round((qry.Fields[0].AsInteger / lTotalTasks) * 100), 0);
        qry.Close;
      end
      else
        lUtilization := 0;
      lblutilizationperc.Text := IntToStr(lUtilization) + '%';
      AnimatePulse(lblutilizationperc);
      Series2.Clear;
      Series2.Add(lUtilization, '');
      Series2.Add(100 - lUtilization, '');

      qry.SQL.Text := 'SELECT Format(s.StartTime, ''yyyy-mm-dd''), COUNT(t.TaskID) FROM tblTasks t INNER JOIN tblSchedule s ON t.TaskID = s.TaskID ' +
                      'WHERE t.ManagerID = :pManagerID AND s.StartTime >= :pStartDate AND s.StartTime < :pEndDate ' +
                      'GROUP BY Format(s.StartTime, ''yyyy-mm-dd'')';
      qry.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
      qry.Parameters.ParamByName('pStartDate').Value := lStartDate;
      qry.Parameters.ParamByName('pEndDate').Value := lEndDate;
      qry.Open;
      while not qry.Eof do
      begin
        lDailyTaskCounts.Add(qry.Fields[0].AsString + '=' + qry.Fields[1].AsString);
        qry.Next;
      end;
      qry.Close;

      qry.SQL.Text := 'SELECT Format(s.StartTime, ''yyyy-mm-dd''), COUNT(t.TaskID) FROM tblTasks t INNER JOIN tblSchedule s ON t.TaskID = s.TaskID ' +
                      'WHERE t.ManagerID = :pManagerID AND t.Priority = ''High'' AND s.StartTime >= :pStartDate AND s.StartTime < :pEndDate ' +
                      'GROUP BY Format(s.StartTime, ''yyyy-mm-dd'')';
      qry.Parameters.ParamByName('pManagerID').Value := CurrentManagerID;
      qry.Parameters.ParamByName('pStartDate').Value := lStartDate;
      qry.Parameters.ParamByName('pEndDate').Value := lEndDate;
      qry.Open;
      while not qry.Eof do
      begin
        lDailyConflictCounts.Add(qry.Fields[0].AsString + '=' + qry.Fields[1].AsString);
        qry.Next;
      end;
      qry.Close;

      TaskChart.Series[0].Clear;
      ConflictsChart.Series[0].Clear;
      lCurrentDate := lStartDate;
      while lCurrentDate < lEndDate do
      begin
        TaskChart.Series[0].Add(StrToIntDef(lDailyTaskCounts.Values[FormatDateTime('yyyy-mm-dd', lCurrentDate)], 0), FormatDateTime('dd mmm', lCurrentDate));
        ConflictsChart.Series[0].Add(StrToIntDef(lDailyConflictCounts.Values[FormatDateTime('yyyy-mm-dd', lCurrentDate)], 0), FormatDateTime('dd mmm', lCurrentDate));
        lCurrentDate := lCurrentDate + 1;
      end;
    finally
      qry.Free;
      lDailyTaskCounts.Free;
      lDailyConflictCounts.Free;
    end;
  finally
    StopLoading;
  end;
end;

procedure TfDashboard.AnimatePulse(LabelObj: TLabel);
var
  animX, animY: TFloatAnimation;
begin
  animX := TFloatAnimation.Create(LabelObj);
  animX.Parent := LabelObj;
  animX.PropertyName := 'Scale.X';
  animX.StartValue := 1.0;
  animX.StopValue := 1.2;
  animX.Duration := 0.12;
  animX.Loop := False;
  animX.AutoReverse := True;
  animX.OnFinish := AnimationFinish;
  animX.Enabled := True;

  animY := TFloatAnimation.Create(LabelObj);
  animY.Parent := LabelObj;
  animY.PropertyName := 'Scale.Y';
  animY.StartValue := 1.0;
  animY.StopValue := 1.2;
  animY.Duration := 0.12;
  animY.Loop := False;
  animY.AutoReverse := True;
  animY.OnFinish := AnimationFinish;
  animY.Enabled := True;
end;

procedure TfDashboard.AnimationFinish(Sender: TObject);
begin
  if Sender is TFloatAnimation then
    if Assigned(TFloatAnimation(Sender).Parent) and (TFloatAnimation(Sender).Parent is TLabel) then
    begin
      TLabel(TFloatAnimation(Sender).Parent).Scale.X := 1.0;
      TLabel(TFloatAnimation(Sender).Parent).Scale.Y := 1.0;
    end;
  Sender.Free;
end;

procedure TfDashboard.GlowRectangle(Rect: TRectangle);
begin
  TAnimator.AnimateFloat(Rect, 'Opacity', 0.7, 0.08);
  TAnimator.AnimateFloat(Rect, 'Opacity', 1.0, 0.12);
end;

procedure TfDashboard.StyleAllCharts;
begin
  with TaskChart do
  begin
    Color := TAlphaColorRec.Null;
    Gradient.Visible := False;
    BevelOuter := bvNone;
    Legend.Visible := False;
    Title.Visible := False;
    Foot.Visible := False;
    Walls.Back.Visible := False;
    View3D := False;
    Axes.Left.Visible := False;
    Axes.Bottom.Visible := True;
    Axes.Bottom.Axis.Visible := False;
    Axes.Bottom.LabelsFont.Color := TAlphaColorRec.Gray;
    Axes.Bottom.Grid.Visible := False;
  end;
  with Series1 do
  begin
    Color := TAlphaColor($FF007AFF);
    Pen.Visible := False;
    Marks.Visible := True;
    Marks.Font.Color := TAlphaColorRec.Gray;
    Marks.Arrow.Visible := False;
    Marks.Frame.Visible := False;
    Marks.Brush.Kind := TBrushKind.None;
    Marks.Style := smsValue;
  end;

  with ConflictsChart do
  begin
    Color := TAlphaColorRec.Null;
    Gradient.Visible := False;
    BevelOuter := bvNone;
    Legend.Visible := False;
    Title.Visible := False;
    Foot.Visible := False;
    Walls.Back.Visible := False;
    View3D := False;
    Axes.Left.Visible := False;
    Axes.Bottom.Visible := True;
    Axes.Bottom.Axis.Visible := False;
    Axes.Bottom.LabelsFont.Color := TAlphaColorRec.Gray;
    Axes.Bottom.Grid.Visible := False;
  end;
  with BarSeries1 do
  begin
    Color := TAlphaColor($FFD9534F);
    Pen.Visible := False;
    Marks.Visible := True;
    Marks.Font.Color := TAlphaColorRec.Gray;
    Marks.Arrow.Visible := False;
    Marks.Frame.Visible := False;
    Marks.Brush.Kind := TBrushKind.None;
    Marks.Style := smsValue;
  end;

  with UtilizationChart do
  begin
    Color := TAlphaColorRec.Null;
    Gradient.Visible := False;
    BevelOuter := bvNone;
    Legend.Visible := False;
    Title.Visible := False;
    Foot.Visible := False;
    Walls.Back.Visible := False;
    View3D := False;
  end;
  with Series2 do
  begin
    Pen.Visible := False;
    Marks.Visible := False;
    Circled := True;
    DonutPercent := 80;
    RotationAngle := 240;
    ValueColor[0] := TAlphaColor($FFD9534F);
    ValueColor[1] := TAlphaColor($FF3C3C3C);
  end;
end;

end.
