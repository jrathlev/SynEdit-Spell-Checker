(* Delphi program
   Demo for spell checking with SynEdit using Hunspell
   ===================================================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Vers. 1.0 - September 2019
   Vers. 1.1 - June 2022 - applicable for 32 and 64 bit compiling
   *)

unit SpellDemoMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, SynEditSpell, Vcl.Menus,
  Vcl.StdCtrls, SynEditHighlighter, SynHighlighterHtml, Vcl.ComCtrls,
  Vcl.Buttons, Vcl.ExtCtrls;

type
  TfrmMain = class(TForm)
    SynEditText: TSynEdit;
    SynEditHtml: TSynEdit;
    pmText: TPopupMenu;
    pmiAddToDictionary: TMenuItem;
    pmiMisspelling: TMenuItem;
    pmiSuggestions: TMenuItem;
    pmiSelectLanguage: TMenuItem;
    pmiNoSpellcheck: TMenuItem;
    SynHTMLSyn: TSynHTMLSyn;
    pcEditor: TPageControl;
    tsText: TTabSheet;
    tsHtml: TTabSheet;
    StatusBar: TStatusBar;
    paTop: TPanel;
    bbFont: TBitBtn;
    FontDialog: TFontDialog;
    procedure FormCreate(Sender: TObject);
    procedure pmTextPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pmiAddToDictionaryClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmiMisspellingClick(Sender: TObject);
    procedure pcEditorChange(Sender: TObject);
    procedure pmiNoSpellcheckClick(Sender: TObject);
    procedure bbFontClick(Sender: TObject);
  private
    { Private-Deklarationen }
    ActiveEditor : TSynEdit;
    procedure ShowStatus (ALangId : word);
    procedure InitDictionary (AEditor : TSynEdit);
    procedure SelectDictLangClick(Sender: TObject);
    procedure SuggestionOnClick(Sender: TObject);
  public
    { Public-Deklarationen }
    SynEditSpellCheck : TSynEditSpellCheck;
    DictLangList      : TStringList;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  mi      : TMenuItem;
  i       : integer;
begin
  SynEditSpellCheck:=TSynEditSpellCheck.Create(self,[caText]);
  with SynEditSpellCheck do begin
    UseUserDictionary:=true;
//    UnderlineColor:=clGreen;
//    UnderlineStyle:=usCorelWordPerfect;
    Options:=[sscoAutoSpellCheck, sscoHourGlass, sscoIgnoreSingleChars, sscoSuggestWords];
    LoadDictionaries(ExtractFilePath(Application.ExeName)+'dict\');
    DictLangList:=TStringList.Create;
    if GetDictLanguages(DictLangList) then begin
      with DictLangList do for i:=0 to Count-1 do begin
        mi:=TMenuItem.Create(Self);
        with mi do begin
          Caption:=Strings[i];
          GroupIndex:=82;
          RadioItem:=true;
          Tag:=integer(Objects[i]);
          OnClick:=SelectDictLangClick;
          end;
        pmiSelectLanguage.Add(mi);
        end;
      end;
    end;
  TDrawAutoSpellCheckPlugin.Create(SynEditHtml,SynEditSpellCheck);
  SynEditHtml.Tag:=1031;   // German
//  SynEditHtml.Highlighter.AdditionalIdentChars:=['ä','ö','ü','Ä','Ö','Ü','ß'];
  SynEditHtml.Highlighter.AdditionalIdentChars:=SynEditHtml.MakeCharArray('#$C0..#$D6,#$D8..#$F6,#$F8..#$FF');
  SynEditHtml.Highlighter.AdditionalWordBreakChars:=[#$30F,#$30B];
  SynEditText.AdditionalWordBreakChars:=[#$30F,#$30B];
  TDrawAutoSpellCheckPlugin.Create(SynEditText,SynEditSpellCheck);
  SynEditText.Tag:=0;      // no language assigned
  ActiveEditor:=nil;
  end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SynEditSpellCheck.Free;
  DictLangList.Free;
  end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  pcEditor.ActivePageIndex:=0;
  InitDictionary(SynEditHtml);
  end;

procedure TfrmMain.ShowStatus (ALangId : word);
begin
  StatusBar.SimpleText:=' Language: '+GetLanguageName(AlangId);
  end;

procedure TfrmMain.InitDictionary (AEditor : TSynEdit);
var
  i : integer;
begin
  ActiveEditor:=AEditor;
  if assigned(ActiveEditor) then begin
    SynEditSpellCheck.SelectDictionary(ActiveEditor.Tag);
    with pmiSelectLanguage do for i:=0 to Count-1 do if Items[i].Tag=ActiveEditor.Tag then begin
      Items[i].Checked:=true; System.Break;
      end;
    ShowStatus(ActiveEditor.Tag);
    end
  else begin
    pmiNoSpellcheck.Checked:=true;
    ShowStatus(0);
    end;
  end;

procedure TfrmMain.pcEditorChange(Sender: TObject);

  function GetEditor : TSynEdit;
  var
    j : integer;
  begin
    Result:=nil;
    with pcEditor.ActivePage do for j:=0 to ControlCount-1 do
        if Controls[j] is TSynEdit then begin
      Result:=Controls[j] as TSynEDit;
      Break;
      end;
    end;

begin
  InitDictionary(GetEditor);
  end;

procedure TfrmMain.bbFontClick(Sender: TObject);
begin
  if assigned(ActiveEditor) then with FontDialog do begin
    Font:=ActiveEditor.Font;
    if Execute then ActiveEditor.Font:=Font;
    end;
  end;

procedure TfrmMain.pmiAddToDictionaryClick(Sender: TObject);
var
  xy : TBufferCoord;
begin
  if assigned(ActiveEditor) then with ActiveEditor do begin
    xy:=CaretXY;
    SynEditSpellCheck.AddDictWord(GetWordAtRowCol(xy));
    CaretXY:=xy;
    Invalidate;
    end;
  end;

procedure TfrmMain.pmiMisspellingClick(Sender: TObject);
var
  sWord,sToken : string;
  PosXY : TBufferCoord;
  Attri : TSynHighlighterAttributes;
  fnd   : boolean;
begin
  if assigned(ActiveEditor) then with ActiveEditor do begin
    fnd:=false; PosXY:=CaretXY;
    repeat
      PosXY:=NextWordPosEx(PosXY);
      if PosXY.Char>0 then begin
        sWord:=GetWordAtRowCol(PosXY);
        with SynEditSpellCheck do if Highlighter = nil then fnd:=not CheckWord(sWord)
        else begin
          if not GetHighlighterAttriAtRowCol(PosXY,sToken,Attri) then
            Attri:=Highlighter.WhitespaceAttribute;
          fnd:=Assigned(Attri) and CheckHighlighterAttribute(Attri.Name) and
            not CheckWord(sWord);
          end;
        end;
      until fnd or (PosXY.Line>=Lines.Count);
    if fnd then CaretXY:=PosXY
    else begin
      Beep;
      ShowMessage('No more misspellings found!');
      end;
    end;
  end;

procedure TfrmMain.pmiNoSpellcheckClick(Sender: TObject);
begin
  SelectDictLangClick(Sender);
  end;

procedure TfrmMain.pmTextPopup(Sender: TObject);
var
  sword : string;
  mnu   : TMenuItem;
  i     : Integer;
  suggList: TStringList;
begin
//  with Sender as TPopupMenu do ActiveEditor:=TSynEdit(PopupComponent);
  if assigned(ActiveEditor) then begin
    with ActiveEditor do sword:=GetWordAtRowCol(CaretXY);
    pmiAddToDictionary.Visible:=(not SynEditSpellCheck.CheckWord(sword)) and (Trim(sword) <> '');
    with pmiSuggestions do begin
      while Count>0 do Items[Count-1].Free;
      Visible:=false;
      end;
    if length(Trim(sword))=0 then Exit;
    suggList := TStringList.Create;
    if pmiAddToDictionary.Visible and (SynEditSpellCheck.GetSuggestions(sword,suggList)>0) then begin
      for i := 0 to suggList.Count-1 do begin
        mnu := TMenuItem.Create(Self);
        if suggList.Strings[i]<>sword then begin
          mnu.Caption:=suggList.Strings[i];
          mnu.OnClick:=SuggestionOnClick;
          pmiSuggestions.Add(mnu);
          end
        else mnu.Free;
        end;
      pmiSuggestions.Visible:=pmiSuggestions.Count>0;
      end;
    pmiMisspelling.Visible:=true;
    pmiSelectLanguage.Visible:=true;
    suggList.Free;
    end;
  end;

procedure TfrmMain.SuggestionOnClick(Sender: TObject);
begin
  if assigned(ActiveEditor) then with ActiveEditor do begin
    BlockBegin:=WordStartEx(CaretXY);
    BlockEnd:=WordEndEx(CaretXY);
    SelText:=StringReplace(TMenuItem(Sender).Caption,'&','',[]);
    end;
  end;

procedure TfrmMain.SelectDictLangClick(Sender: TObject);
var
  lid : word;
begin
  with Sender as TMenuItem do begin
    lid:=Tag; Checked:=true;
    end;
  SynEditSpellCheck.SelectDictionary(lid);
  if assigned(ActiveEditor) then with ActiveEditor do begin
    Invalidate;
    Tag:=lid;
    end;
  ShowStatus(lid);
  end;

end.
