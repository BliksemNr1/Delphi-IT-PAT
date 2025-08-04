unit Login_Frm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, dmOpti, Math,
  FMX.DialogService, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
  IdSSLOpenSSL, IdMessage, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdExplicitTLSClientServerBase, IdMessageClient, IdSMTPBase,
  IdSMTP, System.NetEncoding, System.NetConsts, ShellAPI, Windows, System.IOUtils,
  SignUp_FRM, REST.Types, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,
  System.JSON, Dashboard_FRM, Menu_FRM, TaskManagement_FRM, BayManager_FRM, Scheduler_FRM,
  StaffManager_FRM, StaffScheduler_FRM,
  // *** ADDED: Reports form unit ***
  Reports_FRM;

type
  TFLogin = class(TForm)
    rLogo: TRectangle;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    rcLogin: TRectangle;
    Label1: TLabel;
    Rectangle4: TRectangle;
    Label2: TLabel;
    Label3: TLabel;
    lblSignUp: TLabel;
    lblUsername: TEdit;
    StyleBook1: TStyleBook;
    lblPassword: TEdit;
    lblForgotPassword: TLabel;
    PollingTimer: TTimer;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    Button1: TButton;
    procedure lblSignUpClick(Sender: TObject);
    procedure lblForgotPasswordClick(Sender: TObject);
    function GenerateGUIDToken: String;
    procedure PollingTimerTimer(Sender: TObject);
    procedure rcLoginClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FActiveResetToken: string;
  public
    { Public declarations }
    stempMail : String;
  end;

var
  FLogin: TFLogin;
  CurrentManagerID: Integer; // This is needed for the other forms

implementation

{$R *.fmx}

procedure TFLogin.Button1Click(Sender: TObject);
begin
  // This is a shortcut for testing purposes.
  CurrentManagerID := 1;
  ShowMessage('Testing with ManagerID: ' + IntToStr(CurrentManagerID) + '. Opening all forms for comparison.');

  Self.Visible := false;

  // --- Position the main forms for testing ---

  // Task Management form on the top-left
  FTaskManagement.Visible := True;
  FTaskManagement.Left := Self.Left;
  FTaskManagement.Top := Self.Top;

  // Bay Manager form to the right of the Task Manager
  BayManager.Visible := True;
  BayManager.Left := FTaskManagement.Left + FTaskManagement.Width + 10;
  BayManager.Top := FTaskManagement.Top;

  // Dashboard form to the right of the Bay Manager
  fDashboard.Visible := True;
  fDashboard.Left := BayManager.Left + BayManager.Width + 10;
  fDashboard.Top := BayManager.Top;

  // Scheduler form below the Task Manager
  Scheduler.Visible := True;
  Scheduler.Left := Self.Left;
  Scheduler.Top := FTaskManagement.Top + FTaskManagement.Height + 40; // 40px for window title bar spacing

  // Staff Manager form to the right of the Scheduler
  StaffManager.Visible := True;
  StaffManager.Left := Scheduler.Left + Scheduler.Width + 10;
  StaffManager.Top := Scheduler.Top;

  // Staff Scheduler form to the right of the Staff Manager
  StaffScheduler.Visible := True;
  StaffScheduler.Left := StaffManager.Left + StaffManager.Width + 10;
  StaffScheduler.Top := StaffManager.Top;

  // *** ADDED: Show and position the Reports form on top of the first form ***
  Reports.Visible := True;
  Reports.Left := FTaskManagement.Left;
  Reports.Top := FTaskManagement.Top;
end;

function TFLogin.GenerateGUIDToken: String;
var
  LGuid : TGUID;
begin
  CreateGUID(LGuid);
  Result := GUIDToString(LGuid);
  Result := StringReplace(Result, '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

procedure TFLogin.lblForgotPasswordClick(Sender: TObject);
var
  sTempMail, sResetURL, sUsername, ScriptFile, ScriptText: string;
begin
  FActiveResetToken := GenerateGUIDToken;
  sTempMail := InputBox('Email', 'Please enter your email to receive your reset link', '');

  if sTempMail.IsEmpty then
    Exit;

  with dmopti.FmOpti do
  begin
    if not optimanagers.Active then
      optimanagers.Open;

    if optimanagers.Locate('Email', sTempMail, []) then
    begin
      sUsername := optimanagers['Username'];

      optimanagers.Edit;
      Optimanagers['ResetToken'] := FActiveResetToken;
      optimanagers.Post;

      sResetURL := 'https://password-reset-fc239.web.app/?token=' + FActiveResetToken;

      ScriptText :=
        '$secpass = ConvertTo-SecureString ''egpr zimp tsku egea'' -AsPlainText -Force' + sLineBreak +
        '$cred = New-Object System.Management.Automation.PSCredential(''optiflowZar@gmail.com'', $secpass)' + sLineBreak +
        '$body = @"' + sLineBreak +
        '<!DOCTYPE html><html><head><meta charset=''UTF-8''></head>' +
        '<body style=''font-family:Segoe UI, sans-serif; background:#f4f4f4; padding:50px;''>' +
        '<table style=''max-width:600px; margin:auto; background:#fff; padding:40px; border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.1);''>' +
        '<tr><td style=''text-align:center;''><img src=''https://i.imgur.com/IQgAVtE.png'' alt=''OptiFlow Logo'' style=''max-width:300px; height:auto; margin-bottom:30px;''></td></tr>' +
        '<tr><td><h1 style=''font-size:28px; color:#000; margin-bottom:10px;''>Hello ' + sUsername + ',</h1>' +
        '<p style=''font-size:18px; color:#000; margin:0 0 20px 0;''>A password reset was requested for your OptiFlow account.</p>' +
        '<p style=''font-size:18px; color:#000; margin:0 0 20px 0;''>Click the button below to set a new password:</p>' +
        '<div style=''text-align:center;''><a href="' + sResetURL + '" target="_blank" style=''font-size:24px; font-weight:bold; background-color:#FFC107; color:#1D2429; padding:15px 30px; text-decoration:none; border-radius:8px; display:inline-block;''>Reset Password</a></div>' +
        '<p style=''color:#555; font-size:13px; margin-top:30px;''>If you did not request a password reset, please ignore this email.</p>' +
        '</td></tr></table></body></html>' + sLineBreak +
        '"@' + sLineBreak +
        'Send-MailMessage -To "' + sTempMail + '" -From "optiflowZar@gmail.com" -Subject "Reset Your OptiFlow Password" -Body $body -BodyAsHtml -SmtpServer "smtp.gmail.com" -Port 587 -UseSsl -Credential $cred';

      ScriptFile := TPath.Combine(TPath.GetTempPath, 'OptiFlowReset.ps1');
      TFile.WriteAllText(ScriptFile, ScriptText, TEncoding.UTF8);

      ShellExecute(0, 'open', 'powershell.exe',
        PChar('-ExecutionPolicy Bypass -File "' + ScriptFile + '"'),
        nil, SW_HIDE);

      PollingTimer.Enabled := True;
      ShowMessage('Your reset link has been sent to your email.');
    end
    else
    begin
      TDialogService.MessageDialog('No Such Email Exists', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    end;
  end;
end;

procedure TFLogin.lblSignUpClick(Sender: TObject);
begin
  Self.Visible := false;
  FSignUp.Visible := True;
  FSignUp.Left := Self.Left;
  FSignUp.Top := Self.Top;
end;

procedure TFLogin.PollingTimerTimer(Sender: TObject);
var
  LJSON, LFields, LPasswordObject: TJSONObject;
  LNewPlainTextPass: string;
begin
  PollingTimer.Enabled := False;

  if FActiveResetToken.IsEmpty then
  begin
    PollingTimer.Enabled := True;
    Exit;
  end;

  RESTRequest1.Resource := '/v1/projects/password-reset-fc239/databases/(default)/documents/pendingResets/' + FActiveResetToken;
  RESTRequest1.Method := TRESTRequestMethod.rmGET;

  try
    RESTRequest1.Execute;
  except
    on E: Exception do
    begin
      PollingTimer.Enabled := True;
      Exit;
    end;
  end;

  if RESTResponse1.StatusCode = 200 then
  begin
    LJSON := TJSONObject.ParseJSONValue(RESTResponse1.Content) as TJSONObject;
    try
      if LJSON.TryGetValue('fields', LFields) and (LFields is TJSONObject) then
      begin
        if LFields.TryGetValue('newPlainTextPassword', LPasswordObject) and (LPasswordObject is TJSONObject) then
        begin
          LNewPlainTextPass := LPasswordObject.GetValue<string>('stringValue');

          if not dmopti.FmOpti.optimanagers.Active then
            dmopti.FmOpti.optimanagers.Open;

          if dmopti.FmOpti.optimanagers.Locate('ResetToken', FActiveResetToken, []) then
          begin
              dmopti.FmOpti.optimanagers.Edit;
              dmopti.FmOpti.optimanagers['Password'] := LNewPlainTextPass;
              dmopti.FmOpti.optimanagers['ResetToken'] := '';
              dmopti.FmOpti.optimanagers.Post;

              RESTRequest1.Method := TRESTRequestMethod.rmDELETE;
              RESTRequest1.Execute;

              ShowMessage('Your password has been successfully updated!');
              FActiveResetToken := '';
              Exit;
          end;
        end;
      end;
    finally
      LJSON.Free;
    end;
  end;

  PollingTimer.Enabled := True;
end;

procedure TFLogin.rcLoginClick(Sender: TObject);
var
  bLoginSuccess: Boolean;
begin
  if (lblUsername.text = 'Username') or (lblUsername.text = '') or (lblPassword.text = '') or (lblpassword.text = 'Password') then
  begin
    TDialogService.MessageDialog('Please Enter Your Details', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    Exit;
  end;

  bLoginSuccess := False;
  with dmopti.FmOpti do
  begin
    if not optiManagers.Active then
      optiManagers.Open;

    optiManagers.First;
    while not optiManagers.Eof do
    begin
      if (optiManagers.FieldByName('Username').AsString = lblUsername.Text) and (optiManagers.FieldByName('Password').AsString = lblPassword.Text) then
      begin
        CurrentManagerID := optiManagers.FieldByName('ManagerID').AsInteger;
        bLoginSuccess := True;
        Break;
      end;
      optiManagers.Next;
    end;

    if bLoginSuccess then
    begin
      ShowMessage('You have successfully logged in. Your Manager ID is: ' + IntToStr(CurrentManagerID));
      Self.Visible := False;
      fDashboard.Visible := True;
      fDashboard.Left := Self.Left;
      fDashboard.Top := Self.Top;
    end
    else
    begin
      TDialogService.MessageDialog('Invalid username or password.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    end;
  end;
end;

end.

