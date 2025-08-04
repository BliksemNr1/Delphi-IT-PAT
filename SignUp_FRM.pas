unit SignUp_FRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, dmOpti, Math,
  FMX.DialogService, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
  IdSSLOpenSSL, IdMessage, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdExplicitTLSClientServerBase, IdMessageClient, IdSMTPBase,
  IdSMTP, System.NetEncoding, System.NetConsts,ShellAPI, Windows,System.IOUtils;

type
{ ----Form-Components---- }
  TFSignUp = class(TForm)
    rEmail: TRectangle;
    lblEmail: TEdit;
    rUsername: TRectangle;
    lblUsername: TEdit;
    rPassword: TRectangle;
    lblPassword: TEdit;
    Z: TRectangle;
    lblRegister: TLabel;
    lblError: TLabel;
    rLogo: TRectangle;
    rCode: TRectangle;
    lblCode: TEdit;
    rCompany: TRectangle;
    lblCompany: TEdit;
    Button1: TButton;
    rEye: TRectangle;
    rBack: TRectangle;
    lblreset: TLabel;
    procedure FormCreate(Sender: TObject);

    { -----Key-Strokes-Detect----- }





    procedure lblRegisterClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure lblcompanyKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure lblEmailKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure lblUsernameKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure lblcodeKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure lblPasswordKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure rEyeClick(Sender: TObject);
    procedure rBackClick(Sender: TObject);
    procedure lblresetClick(Sender: TObject);

    { -----Email----- }

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FSignUp: TFSignUp;

  { -----Key-Strokes-Detect----- }
  cEdited, eEdited, uEdited, pEdited, vEdited: Boolean;
  { -----Password-Checker----- }
  ContainsS, Hasc, reenter,eyeopen: Boolean;
  sPass : String;

  { -----Email----- }
  Passed: Boolean;
  Icode : integer;
implementation
  uses Login_FRM;
{$R *.fmx}

{ ----Email-Test-Button---- }
procedure TFSignUp.Button1Click(Sender: TObject);
var
  ScriptFile, ScriptText: string;
begin
  icode := RandomRange(1000, 9999);



ScriptText :=
  '$secpass = ConvertTo-SecureString ''egpr zimp tsku egea'' -AsPlainText -Force' + sLineBreak +
  '$cred = New-Object System.Management.Automation.PSCredential(''optiflowZAR@gmail.com'', $secpass)' + sLineBreak +
  '$body = @"' + sLineBreak +
  '<!DOCTYPE html>' + sLineBreak +
  '<html>' + sLineBreak +
  '<head>' + sLineBreak +
  '  <meta charset=''UTF-8''>' + sLineBreak +
  '</head>' + sLineBreak +
  '<body style=''font-family:Segoe UI, sans-serif; background:#f4f4f4; padding:50px;''>' + sLineBreak +
  '  <table style=''max-width:600px; margin:auto; background:#fff; padding:40px; border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.1);''>' + sLineBreak +
  '    <tr>' + sLineBreak +
  '      <td style=''text-align:center;''>' + sLineBreak +
  '        <img src=''https://i.imgur.com/IQgAVtE.png'' alt=''OptiFlow Logo'' style=''max-width:300px; height:auto; margin-bottom:30px;''>' + sLineBreak +
  '      </td>' + sLineBreak +
  '    </tr>' + sLineBreak +
  '    <tr>' + sLineBreak +
  '      <td>' + sLineBreak +
  '        <h1 style=''font-size:28px; color:#000; margin-bottom:10px;''>Hello ' + Trim(lblUsername.Text) + ',</h1>' + sLineBreak +
  '        <p style=''font-size:18px; color:#000; margin:0 0 20px 0;''>Thank you to <strong>' + Trim(lblCompany.Text) + '</strong> for signing up for <strong>OptiFlow</strong>!</p>' + sLineBreak +
  '        <p style=''font-size:18px; color:#000; margin:0 0 10px 0;''>Your verification code is:</p>' + sLineBreak +
  '        <div style=''font-size:36px; font-weight:bold; background:#ff0; padding:15px 30px; display:inline-block; border-radius:8px; color:#000;''>' + IntToStr(icode) + '</div>' + sLineBreak +
  '        <p style=''color:#555; font-size:13px; margin-top:30px;''>If you did not request this code, please ignore this email.</p>' + sLineBreak +
  '      </td>' + sLineBreak +
  '    </tr>' + sLineBreak +
  '  </table>' + sLineBreak +
  '</body>' + sLineBreak +
  '</html>' + sLineBreak +
  '"@' + sLineBreak +
  'Send-MailMessage -To "' + Trim(lblemail.Text) + '" -From "optiflowZAR@gmail.com" -Subject "Your OptiFlow Verification Code" -Body $body -BodyAsHtml -SmtpServer "smtp.gmail.com" -Port 587 -UseSsl -Credential $cred';

ScriptFile := TPath.Combine(TPath.GetTempPath, 'OptiFlowVerify.ps1');
TFile.WriteAllText(ScriptFile, ScriptText, TEncoding.UTF8);

ShellExecute(0, 'open', 'powershell.exe',
  PChar('-ExecutionPolicy Bypass -WindowStyle Hidden -File "' + ScriptFile + '"'),
  nil, SW_HIDE);

end;

{ ----Form-Initialization---- }
procedure TFSignUp.FormCreate(Sender: TObject);
begin
  rCode.Visible := false;
  lblError.Visible := false;

  { -----Password-eye----- }
  reye.Visible := false;
  eyeopen := false;


  { -----Key-Strokes-Detect----- }
  cEdited := false;
  eEdited := false;
  uEdited := false;
  pEdited := false;
  vEdited := false;
  { -----Email----- }
  Passed := false;

  {-----Password-----}
  reenter := false;
end;

{ ----Verification-Code-Input---- }
procedure TFSignUp.lblcodeKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
  var
  a : Integer;
begin
 if not vEdited then
  begin
    lblCode.Text := KeyChar;
    vEdited := True;
    Key := 0;
  end;





end;


{ ----Company-Name-Input---- }
procedure TFSignUp.lblcompanyKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
 if not cEdited then
  begin
    lblCompany.Text := KeyChar;
    cEdited := True;
    Key := 0;
  end;
end;


{ ----Email-Address-Input---- }
procedure TFSignUp.lblEmailKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
 if not eEdited then
  begin
    lblemail.Text := KeyChar;
    eEdited := True;
    Key := 0;
  end;
end;


{ ----Password-Input-and-Validation---- }
procedure TFSignUp.lblPasswordKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);

var
  sErr, sCaracter: String;
  a,b,c: Integer;

begin

{ ----Handle-First-Keypress---- }
  if not pEdited then
  begin


    lblpassword.Text := KeyChar;
    lblpassword.Password := true;
    lblpassword.text := '';
    lblPassword.GoToTextEnd;
    pEdited := True;
    Key := 0;
    reye.Visible := true;
  end;

  ContainsS := false;
  Hasc := false;
  sCaracter := '@#$%&*';

{ ----Check-for-Special-Characters---- }
  for a := 1 to length(lblpassword.text) do
    begin

      for b := 1 to length(sCaracter) do
        begin
          if lblpassword.text[a] = sCaracter[b] then
          begin
            Hasc := True;
          end;
        end;
    end;

{ ----Check-Password-Length---- }
    if length(lblpassword.text) > 7 then
    begin
      ContainsS := True;
    end;

 c := Ord(ContainsS) + Ord(Hasc) * 2;

{ ----Display-Password-Error-Message---- }
 case c of
  0: begin
       lblerror.visible := true;
       lblerror.text := 'Your password needs to Contain at least 8 caracters and a special caracter (@,#,$,%,&,*)';
       end;

  1: begin
       lblerror.visible := true;
       lblerror.text := 'Your password needs to Contain a special caracter (@,#,$,%,&,*)';
       end;

  2: begin
       lblerror.visible := true;
       lblerror.text := 'Your password needs to Contain at least 8 caracters';
       end;

  3: begin
       lblerror.visible := false;
       lblerror.text := '';
       end;
 end;








end;

{ ----Register-Button---- }
procedure TFSignUp.lblRegisterClick(Sender: TObject);
var
ScriptFile, ScriptText: string;

begin

{ ----Check-for-Existing-User-in-Database---- }
  with FmOpti do
  begin
    optiManagers.Open;
    optiManagers.First;
    While not optiManagers.Eof do
    begin
      if optiManagers['CompanyName'] = lblCompany.text then
      begin
        TDialogService.MessageDialog('This Company Name is already in use', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, procedure (const AResult: TModalResult) begin end);
        exit;
      end;
      if optiManagers['Email'] = lblEmail.text then
      begin
        TDialogService.MessageDialog('This email is already in use', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, procedure (const AResult: TModalResult) begin end);
        exit;
      end;
      optiManagers.Next;
    end;
    optiManagers.close;

  end;

{ ----Verify-Code-and-Create-Account---- }
  if Passed = true then
  begin

    if icode = StrToInt(lblCode.text) then
    begin
      Showmessage('Your Account Has been Created');

      with FmOpti do
      begin
        optiManagers.Open;
        optiManagers.append;
        optimanagers['CompanyName'] := lblCompany.text;
        optiManagers['Email'] := lblEmail.Text;
        optimanagers['Username'] := lblUsername.Text;
        optiManagers['Password'] := lblPassword.text;
        optiManagers['CreatedDate'] := Date;
        optimanagers.Post;

      end;
      FSignUp.visible := false;
      FLogin.Visible := True;
      Flogin.Left := FSignUp.left;
      Flogin.top := FSignup.top;
      exit;
    end
    else
    begin
      TDialogService.MessageDialog('The code you entered does not match', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, procedure (const AResult: TModalResult) begin end);
      exit;
    end;

  end;

{ ----Check-Password-Requirements---- }
  if (Hasc = false) or (ContainsS = false) then
  begin
    TDialogService.MessageDialog('Your Password does not meet the criteria', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, procedure (const AResult: TModalResult) begin end);
    exit;
  end;

{ ----Check-for-Empty-Fields---- }
  if (cEdited = false) or (eEdited = false) or (uEdited = false) or
    (pEdited = false) then
  begin
    TDialogService.MessageDialog('All fields have not been entered', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, procedure (const AResult: TModalResult) begin end);
    exit;
  end;

{ ----Handle-Password-Re-entry-Prompt---- }
  if reenter = false then
  begin
   sPass := lblPassword.text;
  TDialogService.ShowMessage('Please re-enter your password');
  lblPassword.text := 'Re-Enter Password';
  lblPassword.Password := false;
  pEdited := false;
  reenter := True;

  lblCompany.Enabled := false;
  lblEmail.Enabled := false;
  lblUsername.Enabled := false;

  exit;

  end;

{ ----Check-if-Passwords-Match---- }
  if lblPassword.text <> sPass then
  begin

    TDialogService.MessageDialog('Passwords do not Match', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, procedure (const AResult: TModalResult) begin end);

    lblPassword.text := 'Password';
    lblPassword.Password := false;
    pEdited := false;

    reenter := false;
    lblCompany.Enabled := True;
    lblEmail.Enabled := True;
    lblUsername.Enabled := True;

    exit;
  end;

{ ----Send-Verification-Email---- }
  Passed := true;
  lblpassword.Enabled := false;
  rCode.Visible := true;

  icode := RandomRange(1000,9999);

ScriptText :=
  '$secpass = ConvertTo-SecureString ''egpr zimp tsku egea'' -AsPlainText -Force' + sLineBreak +
  '$cred = New-Object System.Management.Automation.PSCredential(''optiflowZAR@gmail.com'', $secpass)' + sLineBreak +
  '$body = @"' + sLineBreak +
  '<!DOCTYPE html>' + sLineBreak +
  '<html>' + sLineBreak +
  '<head>' + sLineBreak +
  '  <meta charset=''UTF-8''>' + sLineBreak +
  '</head>' + sLineBreak +
  '<body style=''font-family:Segoe UI, sans-serif; background:#f4f4f4; padding:50px;''>' + sLineBreak +
  '  <table style=''max-width:600px; margin:auto; background:#fff; padding:40px; border-radius:10px; box-shadow:0 4px 12px rgba(0,0,0,0.1);''>' + sLineBreak +
  '    <tr>' + sLineBreak +
  '      <td style=''text-align:center;''>' + sLineBreak +
  '        <img src=''https://i.imgur.com/IQgAVtE.png'' alt=''OptiFlow Logo'' style=''max-width:300px; height:auto; margin-bottom:30px;''>' + sLineBreak +
  '      </td>' + sLineBreak +
  '    </tr>' + sLineBreak +
  '    <tr>' + sLineBreak +
  '      <td>' + sLineBreak +
  '        <h1 style=''font-size:28px; color:#000; margin-bottom:10px;''>Hello ' + Trim(lblUsername.Text) + ',</h1>' + sLineBreak +
  '        <p style=''font-size:18px; color:#000; margin:0 0 20px 0;''>Thank you to <strong>' + Trim(lblCompany.Text) + '</strong> for signing up for <strong>OptiFlow</strong>!</p>' + sLineBreak +
  '        <p style=''font-size:18px; color:#000; margin:0 0 10px 0;''>Your verification code is:</p>' + sLineBreak +
  '        <div style=''font-size:36px; font-weight:bold; background:#ff0; padding:15px 30px; display:inline-block; border-radius:8px; color:#000;''>' + IntToStr(icode) + '</div>' + sLineBreak +
  '        <p style=''color:#555; font-size:13px; margin-top:30px;''>If you did not request this code, please ignore this email.</p>' + sLineBreak +
  '      </td>' + sLineBreak +
  '    </tr>' + sLineBreak +
  '  </table>' + sLineBreak +
  '</body>' + sLineBreak +
  '</html>' + sLineBreak +
  '"@' + sLineBreak +
  'Send-MailMessage -To "' + Trim(lblemail.Text) + '" -From "optiflowZAR@gmail.com" -Subject "Your OptiFlow Verification Code" -Body $body -BodyAsHtml -SmtpServer "smtp.gmail.com" -Port 587 -UseSsl -Credential $cred';

ScriptFile := TPath.Combine(TPath.GetTempPath, 'OptiFlowVerify.ps1');
TFile.WriteAllText(ScriptFile, ScriptText, TEncoding.UTF8);

ShellExecute(0, 'open', 'powershell.exe',
  PChar('-ExecutionPolicy Bypass -WindowStyle Hidden -File "' + ScriptFile + '"'),
  nil, SW_HIDE);




  Tdialogservice.ShowMessage('If the email exists you will recieve your verification code');
  lblRegister.text := 'Verify Code';


end;




{ -----Reset-Button----- }
procedure TFSignUp.lblresetClick(Sender: TObject);
begin

lblCompany.Text := 'Company';
cEdited := false;
lblCompany.Enabled := true;

lblEmail.text := 'Email';
eEdited := false;
lblEmail.Enabled := true;

lblusername.text := 'Username';
uEdited := false;
lblusername.Enabled := true;

lblPassword.Password := false;
lblPassword.text := 'Password';
pEdited := false;
lblPassword.Enabled := true;
lblerror.Visible := false;

rCode.Visible := false;
lblError.Visible := false;

{ -----Password-eye----- }
reye.Visible := false;
eyeopen := false;

{ -----Email----- }
Passed := false;

{-----Password-----}
reenter := false;

end;

{ ----Username-Input---- }
procedure TFSignUp.lblUsernameKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
 if not uEdited then
  begin
    lblusername.Text := KeyChar;
    uEdited := True;
    Key := 0;
  end;
end;

{ ----Back-Button---- }
procedure TFSignUp.rBackClick(Sender: TObject);
begin
FSignUp.visible := false;
FLogin.Visible := True;
Flogin.Left := FSignUp.left;
Flogin.top := FSignup.top;
end;

{ ----Password-Visibility-Toggle---- }
procedure TFSignUp.rEyeClick(Sender: TObject);
begin



if eyeopen = false then
begin
eyeopen := true;
reye.fill.Bitmap.bitmap.LoadFromFile('ICONS\eye off.png');
lblpassword.Password := false;
exit;
end;

if eyeopen = true then
begin
  eyeopen := false;
  reye.fill.Bitmap.bitmap.LoadFromFile('ICONS\eye.png');
  lblpassword.Password := true;
  exit;
end;




end;

end.
