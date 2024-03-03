(* Delphi unit
   Spell checker for SynEdit
   =========================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   based on SynSpellCheck by Jacob Dybala
   https://sourceforge.net/projects/hunspell/files/Misc/

   and NHunspell by Thomas Maierhofer
   http://nhunspell.sourceforge.net

   Vers. 1.0 - August 2019
   Vers. 1.1 - September 2021: Fixed underlining of bad words if WordWrap is enabled
   Vers. 1.2 - June 2022: Fixed issue on compiling as 64 bit application
   last modified: August 2023
   *)

unit SynEditSpell;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, System.Contnrs,
  Vcl.Graphics, Vcl.Controls,
  SynEdit, SynEditHighlighter, SynEditMiscProcs, SynEditTypes, HunSpellLib;

type
  TLanguage = record
    Id : word;
    Shortname : string;
    end;

  TLoadedDict = record
    Id : word;
    Shortname,Filename : string;
    mAff,mDic : TMemoryStream;
    end;

  TLoadedDicts = array of TLoadedDict;

  TUnderlineStyle = (usCorelWordPerfect, usMicrosoftWord);
  TCheckAttribute = (caText,caComment,caString,caDocumentation);
  TCheckAttributes = set of TCheckAttribute;

const
  AllAttributes = [caText,caComment,caString,caDocumentation];

type
  TSynEditSpellCheck = class;

  TDrawAutoSpellCheckPlugin = class(TSynEditPlugin)
  private
    { Procedures }
  protected
    FEditor : TSynEdit;
    FSynEditSpellCheck : TSynEditSpellCheck;
    { Procedures }
    procedure AfterPaint(ACanvas : TCanvas; const AClip: TRect; FirstLine,LastLine : Integer); override;
  public
    constructor Create(AOwner : TCustomSynEdit; ASynEditSpellCheck : TSynEditSpellCheck);
    { Properties }
    end;

  TSynSpellCheckOption = (
    sscoAutoSpellCheck,
    sscoGoUp,
    sscoHideCursor,
    sscoHourGlass,
    sscoIgnoreSingleChars,
    sscoStartFromCursor,
    sscoSuggestWords);
  TSynSpellCheckOptions = set of TSynSpellCheckOption;

  { Procedure types }
  TOnLoadDict = procedure(Sender: TObject; AId : word) of object;
  TOnAddWord = procedure(Sender: TObject; AWord: String) of object;
  TOnCheckWord = procedure(Sender: TObject; AWord: String;
    ASuggestions: TStringList; var ACorrectWord: String; var AAction: Integer;
    const AUndoEnabled: Boolean = True) of object;

  TSynEditSpellCheck = class(TComponent)
  private
    FHunspellHandle: Pointer;
    FEnabled : boolean;
    FLangId : word;
    FBusy, FModified, FUseUserDictionary : Boolean;
    FDictPath, FUserFileName, FUserDictPath : String;
    FUnderlineColor : TColor;
    FUnderlineStyle : TUnderlineStyle;
    FUserDict : TStringList;
    FLoadedDicts : TLoadedDicts;
    FOnAddWord : TOnAddWord;
    FOnAbort, FOnDictSelect, FOnDictClose, FOnDone, FOnStart : TNotifyEvent;
    FOnDictLoad : TOnLoadDict;
    FOnCheckWord: TOnCheckWord;
    FCheckAttribs : TStringList;
    FOptions : TSynSpellCheckOptions;
    { Functions }
    function GetDefaultDictionaryDir : string;
    function GetUserDictionaryDir : string;
    function GetDictLanguageName : string;
    { Procedures }
    procedure UserDictChange (Sender : TObject);
    procedure SetUnderlineColor (Value: TColor);
    procedure SetUnderlineStyle (Value: TUnderlineStyle);
  public
    constructor Create (AOwner : TComponent; ACheckAttri : TCheckAttributes = AllAttributes);
    destructor Destroy; override;

    function GetLanguageIndex (const AShortName : string) : integer;
    function GetDictIndex (LangId : word) : integer;
    function LoadDictionaries (const APath : string) : boolean;
    function SelectDictionary (const LangShortname : String) : boolean; overload;
    function SelectDictionary (LangId : word) : boolean; overload;
    procedure CloseDictionary;
    procedure SaveUserDictionary;
    function GetDictLanguages (ALangList : TStringList) : boolean;

    function CheckHighlighterAttribute (const AttributeName : string) : boolean;
    function CheckWord (const AWord : String): Boolean;
    function GetSuggestions (const AWord : String; SuggestionList: TStringList): Integer;
    procedure AddDictWord (const AWord : String);
    function SpellCheck (AEditor : TSynEdit) : boolean;
    property UserDict : TStringList read FUserDict write FUserDict;
  published
    { Properties }
    property Busy: Boolean read FBusy default False;
    property Enabled : boolean read FEnabled;
    property LanguageName : String read GetDictLanguageName;
    property DictionaryPath: String read FDictPath;
    property Modified: Boolean read FModified write FModified default False;
    property Options: TSynSpellCheckOptions read FOptions write FOptions;
    property UnderlineColor: TColor read FUnderlineColor write SetUnderlineColor default clRed;
    property UnderlineStyle: TUnderlineStyle read FUnderlineStyle
      write SetUnderlineStyle default usMicrosoftWord;
    property UserDirectory: String read GetUserDictionaryDir write FUserDictPath;
    property UseUserDictionary: Boolean read FUseUserDictionary write
      FUseUserDictionary default True;
    { Events }
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnAddWord: TOnAddWord read FOnAddWord write FOnAddWord;
    property OnCheckWord: TOnCheckWord read FOnCheckWord write FOnCheckWord;
    property OnDictLoad: TOnLoadDict read FOnDictLoad write FOnDictLoad;
    property OnDictSelec: TNotifyEvent read FOnDictSelect write FOnDictSelect;
    property OnDictClose: TNotifyEvent read FOnDictClose write FOnDictClose;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

function LangIDToLangShortname(ALangID : Word) : string;
function GetLanguageName(LangID: word): String;

procedure Register;

implementation

uses System.Math, System.StrUtils, Vcl.Forms;

const
  extAff = '.aff';
  extDic = '.dic';

  Languages : array[0..108] of TLanguage = (
    (Id: LANG_AFRIKAANS or SUBLANG_DEFAULT shl 10; Shortname: 'af'),
    (Id: LANG_ALBANIAN or SUBLANG_DEFAULT shl 10; Shortname: 'sq'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_SAUDI_ARABIA shl 10; Shortname: 'ar-sa'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_IRAQ shl 10; Shortname: 'ar-iq'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_EGYPT shl 10; Shortname: 'ar-eg'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_LIBYA shl 10; Shortname: 'ar-ly'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_ALGERIA shl 10; Shortname: 'ar-dz'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_MOROCCO shl 10; Shortname: 'ar-ma'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_TUNISIA shl 10; Shortname: 'ar-tn'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_OMAN shl 10; Shortname: 'ar-om'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_YEMEN shl 10; Shortname: 'ar-ye'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_SYRIA shl 10; Shortname: 'ar-sy'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_JORDAN shl 10; Shortname: 'ar-jo'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_LEBANON shl 10; Shortname: 'ar-lb'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_KUWAIT shl 10; Shortname: 'ar-kw'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_UAE shl 10; Shortname: 'ar-ae'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_BAHRAIN shl 10; Shortname: 'ar-bh'),
    (Id: LANG_ARABIC or SUBLANG_ARABIC_QATAR shl 10; Shortname: 'ar-qa'),
    (Id: LANG_BASQUE or SUBLANG_DEFAULT shl 10; Shortname: 'eu'),
    (Id: LANG_BELARUSIAN; Shortname: 'be'),
    (Id: LANG_BULGARIAN; Shortname: 'bg'),
    (Id: LANG_CATALAN; Shortname: 'ca'),
    (Id: LANG_CHINESE; Shortname: 'zh'),
    (Id: LANG_CHINESE or SUBLANG_CHINESE_TRADITIONAL shl 10; Shortname: 'zh-tw'),
    (Id: LANG_CHINESE or SUBLANG_CHINESE_SIMPLIFIED shl 10; Shortname: 'zh-cn'),
    (Id: LANG_CHINESE or SUBLANG_CHINESE_HONGKONG shl 10; Shortname: 'zh-hk'),
    (Id: LANG_CHINESE or SUBLANG_CHINESE_SINGAPORE shl 10; Shortname: 'zh-sg'),
    (Id: LANG_CROATIAN; Shortname: 'hr'),
    (Id: LANG_CZECH; Shortname: 'cs'),
    (Id: LANG_DANISH; Shortname: 'da'),
    (Id: LANG_DUTCH or SUBLANG_DUTCH shl 10; Shortname: 'nl-nl'),
    (Id: LANG_DUTCH or SUBLANG_DUTCH_BELGIAN shl 10; Shortname: 'nl-be'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_US shl 10; Shortname: 'en-us'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_UK shl 10; Shortname: 'en-uk'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_UK shl 10; Shortname: 'en-gb'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_AUS shl 10; Shortname: 'en-au'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_CAN shl 10; Shortname: 'en-ca'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_NZ shl 10; Shortname: 'en-nz'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_EIRE shl 10; Shortname: 'en-ie'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_SOUTH_AFRICA shl 10; Shortname: 'en-za'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_JAMAICA shl 10; Shortname: 'en-jm'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_CARIBBEAN shl 10; Shortname: 'en-cb'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_BELIZE shl 10; Shortname: 'en-bz'),
    (Id: LANG_ENGLISH or SUBLANG_ENGLISH_TRINIDAD shl 10; Shortname: 'en-tt'),
    (Id: LANG_ESTONIAN; Shortname: 'et'),
    (Id: LANG_FAEROESE; Shortname: 'fo'),
    (Id: LANG_FARSI; Shortname: 'fa'),
    (Id: LANG_FINNISH; Shortname: 'fi'),
    (Id: LANG_FRENCH or SUBLANG_FRENCH shl 10; Shortname: 'fr-fr'),
    (Id: LANG_FRENCH or SUBLANG_FRENCH_BELGIAN shl 10; Shortname: 'fr-be'),
    (Id: LANG_FRENCH or SUBLANG_FRENCH_CANADIAN shl 10; Shortname: 'fr-ca'),
    (Id: LANG_FRENCH or SUBLANG_FRENCH_SWISS shl 10; Shortname: 'fr-ch'),
    (Id: LANG_FRENCH or SUBLANG_FRENCH_LUXEMBOURG shl 10; Shortname: 'fr-lu'),
    (Id: LANG_GERMAN or SUBLANG_GERMAN shl 10; Shortname: 'de-de'),
    (Id: LANG_GERMAN or SUBLANG_GERMAN_SWISS shl 10; Shortname: 'de-ch'),
    (Id: LANG_GERMAN or SUBLANG_GERMAN_AUSTRIAN shl 10; Shortname: 'de-at'),
    (Id: LANG_GERMAN or SUBLANG_GERMAN_LUXEMBOURG shl 10; Shortname: 'de-lu'),
    (Id: LANG_GERMAN or SUBLANG_GERMAN_LIECHTENSTEIN shl 10; Shortname: 'de-li'),
    (Id: LANG_GREEK; Shortname: 'el'),
    (Id: LANG_HEBREW; Shortname: 'he'),
    (Id: LANG_HUNGARIAN; Shortname: 'hu'),
    (Id: LANG_ICELANDIC; Shortname: 'is'),
    (Id: LANG_INDONESIAN; Shortname: 'id'),
    (Id: LANG_ITALIAN or SUBLANG_ITALIAN shl 10; Shortname: 'it-it'),
    (Id: LANG_ITALIAN or SUBLANG_ITALIAN_SWISS shl 10; Shortname: 'it-ch'),
    (Id: LANG_JAPANESE; Shortname: 'ja'),
    (Id: LANG_KOREAN or SUBLANG_KOREAN shl 10; Shortname: 'ko'),
    (Id: LANG_KOREAN or SUBLANG_KOREAN shl 10; Shortname: 'ko-ko'),
    (Id: LANG_KOREAN or SUBLANG_KOREAN_JOHAB shl 10; Shortname: 'ko-kp'),
    (Id: LANG_LATVIAN or SUBLANG_DEFAULT shl 10; Shortname: 'lv'),
    (Id: LANG_LITHUANIAN or SUBLANG_DEFAULT shl 10; Shortname: 'lt'),
    (Id: LANG_NORWEGIAN or SUBLANG_NORWEGIAN_BOKMAL shl 10; Shortname: 'no'),
    (Id: LANG_NORWEGIAN or SUBLANG_NORWEGIAN_NYNORSK shl 10; Shortname: 'no-no'),
    (Id: LANG_POLISH or SUBLANG_DEFAULT shl 10; Shortname: 'pl'),
    (Id: LANG_PORTUGUESE or SUBLANG_PORTUGUESE shl 10; Shortname: 'pt-pt'),
    (Id: LANG_PORTUGUESE or SUBLANG_PORTUGUESE_BRAZILIAN shl 1; Shortname: 'pt-br'),
    (Id: LANG_ROMANIAN or SUBLANG_DEFAULT shl 10; Shortname: 'ro'),
    (Id: LANG_RUSSIAN or SUBLANG_DEFAULT shl 10; Shortname: 'ru'),
    (Id: LANG_SERBIAN or SUBLANG_DEFAULT shl 10; Shortname: 'sr'),
    (Id: LANG_SERBIAN or SUBLANG_SERBIAN_LATIN shl 10; Shortname: 'sr'),
    (Id: LANG_SERBIAN or SUBLANG_SERBIAN_CYRILLIC shl 10; Shortname: 'sr'),
    (Id: LANG_SLOVAK or SUBLANG_DEFAULT shl 10; Shortname: 'sk'),
    (Id: LANG_SLOVENIAN or SUBLANG_DEFAULT shl 10; Shortname: 'sl'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH shl 10; Shortname: 'es'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_MEXICAN shl 10; Shortname: 'es-mx'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_MODERN shl 10; Shortname: 'es'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_GUATEMALA shl 10; Shortname: 'es-gt'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_COSTA_RICA shl 10; Shortname: 'es-cr'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_PANAMA shl 10; Shortname: 'es-pa'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_DOMINICAN_REPUBLIC; Shortname: 'es-do'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_VENEZUELA shl 10; Shortname: 'es-ve'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_COLOMBIA shl 10; Shortname: 'es-co'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_PERU shl 10; Shortname: 'es-pe'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_ARGENTINA shl 10; Shortname: 'es-ar'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_ECUADOR shl 10; Shortname: 'es-ec'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_CHILE shl 10; Shortname: 'es-cl'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_URUGUAY shl 10; Shortname: 'es-uy'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_PARAGUAY shl 10; Shortname: 'es-py'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_BOLIVIA shl 10; Shortname: 'es-bo'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_EL_SALVADOR shl 10; Shortname: 'es-sv'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_HONDURAS shl 10; Shortname: 'es-hn'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_NICARAGUA shl 10; Shortname: 'es-ni'),
    (Id: LANG_SPANISH or SUBLANG_SPANISH_PUERTO_RICO shl 10; Shortname: 'es-pr'),
    (Id: LANG_SWEDISH or SUBLANG_SWEDISH shl 10; Shortname: 'sv'),
    (Id: LANG_SWEDISH or SUBLANG_SWEDISH_FINLAND shl 10; Shortname: 'sv-fi'),
    (Id: LANG_THAI or SUBLANG_DEFAULT shl 10; Shortname: 'th'),
    (Id: LANG_TURKISH or SUBLANG_DEFAULT shl 10; Shortname: 'tr'),
    (Id: LANG_UKRAINIAN or SUBLANG_DEFAULT shl 10; Shortname: 'uk'),
    (Id: LANG_VIETNAMESE or SUBLANG_DEFAULT shl 10; Shortname: 'vi'));

function LangIDToLangShortname(ALangID : Word) : string;
var
  i: Integer;
begin
  for i:=Low(Languages) to High(Languages) do with Languages[i] do
    if ALangID = Id then begin
      Result:=Shortname;
      Exit;
    end;
  Result:='';
  end;

function LangShortnameToLangID(ALangName : String) : Word;
var
  i: Integer;
begin
  for i:=Low(Languages) to High(Languages) do with Languages[i] do
    if AnsiSameText(Shortname,ALangName) then begin
      Result:=Id;
      Exit;
    end;
  // second try, primary language code only
  ALangName:=copy(ALangName,1,2);
  for i:=Low(Languages) to High(Languages) do with Languages[i] do
    if AnsiSameText(ALangName,Copy(Shortname,1,2)) then begin
      Result:=Id;
      Exit;
    end;
  Result:=LANG_ENGLISH or SUBLANG_ENGLISH_US shl 10;
  end;

function GetLanguageName(LangID: word): String;
var
  Buf: array [0..LOCALE_NAME_MAX_LENGTH-1] of Char;
begin
  VerLanguageName(LangID,@Buf,LOCALE_NAME_MAX_LENGTH-1);
  Result:=Buf;
  if length(Result)=0 then Result:='<?>';
  end;

procedure Register;
begin
  RegisterComponents('SynEdit',[TSynEditSpellCheck]);
  end;

{ TDrawAutoSpellCheckPlugin }

constructor TDrawAutoSpellCheckPlugin.Create (AOwner : TCustomSynEdit;
                                              ASynEditSpellCheck : TSynEditSpellCheck);
begin
  inherited Create(AOwner);
  FEditor:=TSynEdit(AOwner);
  FSynEditSpellCheck:=ASynEditSpellCheck;
  end;

procedure TDrawAutoSpellCheckPlugin.AfterPaint(ACanvas : TCanvas; const AClip : TRect;
  FirstLine, LastLine : Integer);
var
  lh,cx,i     : Integer;
  CurrentWord : String;
  CurrentXY   : TBufferCoord;
  tp          : TPoint;
  sToken      : UnicodeString;
  Attri       : TSynHighlighterAttributes;

  procedure PaintUnderLine;
  var
    MaxX,NewPoint,NewY : Integer;
    mus                : boolean;

    procedure DrawPoint;
    begin
      // Do not draw on gutter.
      // This happens when a word is underlined and part of it is "hidden" under
      // the gutter.
      if tp.X <= FEditor.Gutter.RealGutterWidth(FEditor.CharWidth) then Exit;
      with ACanvas do begin
        if NewY=tp.Y-1 then Pen.Color:=FEditor.Color
        else Pen.Color:=FSynEditSpellCheck.UnderlineColor;
        Pixels[tp.X, NewY]:=Pen.Color;
        end;
      end;

  const
    // Microsoft Word style
    MW_POINTS: array[0..3] of ShortInt=(0, 1, 2, 1);
    // Corel Word Perfect style
    WP_POINTS: array[0..3] of ShortInt=(2, 1, 0, -1);

  begin
    Inc(tp.Y, lh-3);
    NewPoint:=0;
    mus:=FSynEditSpellCheck.UnderlineStyle=usMicrosoftWord;
    if mus then NewY:=tp.Y+MW_POINTS[NewPoint]
    else NewY:=tp.Y+WP_POINTS[NewPoint];
    DrawPoint;
    MaxX:=tp.X+ACanvas.TextWidth(CurrentWord);
    while tp.X <= MaxX do begin
      DrawPoint;
      Inc(NewPoint);
      if mus then begin
        if NewPoint > High(MW_POINTS) then NewPoint:=0
        end
      else begin
        if NewPoint > High(WP_POINTS) then NewPoint:=0;
        end;
      DrawPoint;
      Inc(tp.X);
      if mus then NewY:=tp.Y+MW_POINTS[NewPoint]
      else NewY:=tp.Y+WP_POINTS[NewPoint];
      end;
    end;

begin
  if not Assigned(FSynEditSpellCheck) or not Assigned(FEditor) or
    not FSynEditSpellCheck.Enabled or not(sscoAutoSpellCheck in FSynEditSpellCheck.Options) then Exit;
  lh:=FEditor.LineHeight;
  ACanvas.Font.Assign(FEditor.Font);
// if WordWrap is active FirstLine and Lastline are the index of the dospülayed rows
  for i:=FEditor.RowToLine(FirstLine) to FEditor.RowToLine(LastLine) do begin
    // Paint "Bad Words"
    cx:=1;
    while cx < Length(FEditor.Lines[i-1]) do begin
      CurrentXY:=BufferCoord(cx,i);
      CurrentWord:=FEditor.GetWordAtRowCol(CurrentXY);
      if length(CurrentWord)>0 then begin
        tp:=FEditor.RowColumnToPixels(FEditor.BufferToDisplayPos(CurrentXY));
        if tp.X>ACanvas.ClipRect.Right-ACanvas.ClipRect.Left then Break;
        if Assigned(FEditor.Highlighter) then begin
          if not FEditor.GetHighlighterAttriAtRowCol(CurrentXY, sToken, Attri) then
            Attri:=FEditor.Highlighter.WhitespaceAttribute;
          if Assigned(Attri) and (FSynEditSpellCheck.FCheckAttribs.IndexOf(Attri.Name)>=0) and
            not FSynEditSpellCheck.CheckWord(CurrentWord) then PaintUnderLine;
          end
        else if not FSynEditSpellCheck.CheckWord(CurrentWord) then PaintUnderLine;
        Inc(cx, Length(CurrentWord));
        end;
      Inc(cx);
      end;
    end;
  end;

{ TSynEditSpellCheck }

const
  cpUtf8 = 65001;  // UTF-8 code page

constructor TSynEditSpellCheck.Create (AOwner : TComponent; ACheckAttri : TCheckAttributes);
begin
  inherited Create(AOwner);
  FHunspellHandle:=nil;
  FEnabled:=false; FLangId:=0;
  FUnderlineColor:=clRed;
  FUnderlineStyle:=usMicrosoftWord;
  FBusy:=False;
  FModified:=False;
  FUseUserDictionary:=True;
// User dictionary
  FUserDict:=TStringList.Create;
  with FUserDict do begin
    CaseSensitive:=true;
    Sorted:=true;
    Duplicates:=dupIgnore;
    WriteBOM:=false;
    OnChange:=UserDictChange;
    end;
  FCheckAttribs:=TStringList.Create;
// List of highlighter attributes to be checked
  with FCheckAttribs do begin
    if caText in ACheckAttri then Add('Text');
    if caComment in ACheckAttri then Add('Comment');
    if caString in ACheckAttri then Add('String');
    if caDocumentation in ACheckAttri then Add('Documentation');
    end;
  end;

destructor TSynEditSpellCheck.Destroy;
var
  i : integer;
begin
  CloseDictionary;
  if FLoadedDicts<>nil then for i:=0 to High(FLoadedDicts) do with FLoadedDicts[i] do begin
    mAff.Free; mDic.Free;
    end;
  FUserDict.Free;
  FCheckAttribs.Free;
  inherited;
  end;

procedure TSynEditSpellCheck.SetUnderlineColor (Value : TColor);
begin
  FUnderlineColor:=Value;
  end;

procedure TSynEditSpellCheck.SetUnderlineStyle (Value : TUnderlineStyle);
begin
  FUnderlineStyle:=Value;
  end;

// Get index from language list matching to AShortName
function TSynEditSpellCheck.GetLanguageIndex (const AShortName : string) : integer;
var
  sl : string;
begin
  sl:=copy(AShortName,1,5);
  for Result:=0 to High(Languages) do begin
    if AnsiSameText(sl,Languages[Result].ShortName) then Exit
    else if AnsiSameText(AnsiReplaceStr(sl,'_','-'),Languages[Result].ShortName) then Exit;
    end;
  sl:=copy(AShortName,1,2);
  for Result:=0 to High(Languages) do
    if AnsiSameText(sl,Languages[Result].ShortName) then Exit;
  Result:=-1;
  end;

function TSynEditSpellCheck.GetDictIndex (LangId : word) : integer;
begin
  for Result:=0 to High(FLoadedDicts) do
    if LangId=FLoadedDicts[Result].Id then Exit;
  Result:=-1;
  end;

// Load all dictionaries found in APath
function TSynEditSpellCheck.LoadDictionaries (const APath : string) : boolean;
var
  SearchRec: TSearchRec;
  FindResult,
  n,dcnt : integer;
  sn     : string;
begin
  Result:=false;
  if DirectoryExists(APath) then begin
    FDictPath:=IncludeTrailingBackslash(APath);
    dcnt:=0; FLoadedDicts:=nil;
    FindResult:=FindFirst(FDictPath+'*'+extAff,faAnyFile,SearchRec);
    while (FindResult=0) do begin
      sn:=ChangeFileExt(SearchRec.Name,'');
      n:=GetLanguageIndex(sn);
      if n>=0 then begin
        SetLength(FLoadedDicts,dcnt+1);
        with FLoadedDicts[dcnt] do begin
          Id:=Languages[n].Id;
          Shortname:=Languages[n].Shortname;
          Filename:=sn;
          end;
        inc(dcnt);
        end;
      FindResult:=FindNext(SearchRec);
      end;
    FindClose(SearchRec);
    Result:=dcnt>0;
    if Result then begin  // load dictionaries
      for n:=0 to High(FLoadedDicts) do with FLoadedDicts[n] do begin
        if Assigned(FOnDictLoad) then FOnDictLoad(Self,Id);
        sn:=FDictPath+Filename;
        mAff:=TMemoryStream.Create;
        mAff.LoadFromFile(sn+extAff);
        mDic:=TMemoryStream.Create;
        mDic.LoadFromFile(sn+extDic);
        end;
      end;
    end;
  if length(FUserDictPath)=0 then FUserDictPath:=GetUserDictionaryDir;
  end;

function TSynEditSpellCheck.GetDefaultDictionaryDir : string;
begin
  Result:=GetEnvironmentVariable('APPDATA')+'\SynSpell\';
//  Result:=GetDesktopFolder(CSIDL_APPDATA)+'\SynSpell\';
  end;

function TSynEditSpellCheck.GetUserDictionaryDir : string;
begin
  if FUserDictPath<>'' then Result:=IncludeTrailingBackslash(FUserDictPath)
  else Result:=IncludeTrailingBackslash(GetDefaultDictionaryDir);
  end;

function TSynEditSpellCheck.GetDictLanguageName : string;
begin
  Result:=GetLanguageName(FLangId);
  end;

// Select the current dictionary associated to LangId
function TSynEditSpellCheck.SelectDictionary (LangId : word) : boolean;
var
  n     : integer;
  sn,sl : string;
  fOut  : TextFile;
  FCursor: TCursor;
begin
  Result:=true;
  if LangId=0 then FEnabled:=false
  else if FLangId<>LangId then begin // change dictionary
    n:=GetDictIndex(LangId);
    if n>=0 then begin
      if sscoHourGlass in FOptions then begin
        FCursor:=Screen.Cursor;
        Screen.Cursor:=crHourGlass;
        end;
      if FHunspellHandle<>nil then begin
        CloseDictionary;
        if FUseUserDictionary then SaveUserDictionary;
        end;
      with FLoadedDicts[n] do begin
        FLangId:=Id;
        FHunspellHandle:=HunspellInit(mAff.Memory,mAff.Size,mDic.Memory,mDic.Size,'');
        FUserFileName:=Shortname+'.user.dic';
        end;
      Result:=Assigned(FHunspellHandle);
    // Load user dictionary if present
      FModified:=False;
      if FUseUserDictionary and Result then begin
        sn:=IncludeTrailingBackslash(FUserDictPath)+FUserFileName;
        FUserDict.Clear;
        if FileExists(sn) then begin
          AssignFile(fOut,sn,cpUtf8);
          Reset(fOut);
          while not Eof(fOut) do begin
            ReadLn(fOut,sl);
            sl:=Trim(sl);
            if length(sl)>0 then begin
              FUserDict.Add(sl);
              HunspellAdd(FHunspellHandle,PChar(sl));
              end;
            end;
          CloseFile(fOut);
          end;
        end;
      if sscoHourGlass in FOptions then Screen.Cursor:=FCursor;
      if Assigned(FOnDictSelect) then FOnDictSelect(Self);
      end
    else begin
      Result:=false;
      CloseDictionary;
      FlangId:=0;
      end;
    FEnabled:=Result;
    end
  else FEnabled:=true;
  end;

function TSynEditSpellCheck.SelectDictionary (const LangShortname : String) : boolean;
begin
  Result:=SelectDictionary(LangShortnameToLangID(LangShortname));
  end;

procedure TSynEditSpellCheck.UserDictChange (Sender : TObject);
begin
  FModified:=true;
  end;

procedure TSynEditSpellCheck.CloseDictionary;
begin
  if FUseUserDictionary then SaveUserDictionary;
  if Assigned(FOnDictClose) then FOnDictClose(Self);
  if Assigned(FHunspellHandle) then HunspellFree(FHunspellHandle);
  FHunspellHandle:=nil;
  end;

// Save user dictionary
procedure TSynEditSpellCheck.SaveUserDictionary;
begin
  if FModified then begin
    if DirectoryExists(ExtractFileDir(FUserDictPath)) or
        ForceDirectories(ExtractFileDir(FUserDictPath)) then with FUserDict do if (Count>0) then
      SaveToFile(IncludeTrailingBackslash(FUserDictPath)+FUserFileName,TEncoding.UTF8);
    end;
  FModified:=False;
  end;

// Get list of available dictionaries with full qualified names
function TSynEditSpellCheck.GetDictLanguages (ALangList : TStringList) : boolean;
var
  i : integer;
  w : word;
begin
  for i:=0 to High(FLoadedDicts) do begin
    w:=FLoadedDicts[i].Id;
    ALangList.AddObject(GetLanguageName(w),pointer(w));
    end;
  Result:=ALangList.Count>0;
  end;

// Get suggestions for misspelled word
function TSynEditSpellCheck.GetSuggestions (const AWord : String; SuggestionList : TStringList): Integer;
var
  wrds : PPChar;
begin
  Result:=0;
  if not (sscoSuggestWords in FOptions) then Exit;
  if FEnabled and Assigned(FHunspellHandle) and Assigned(SuggestionList)  then begin
    wrds:=HunspellSuggest(FHunspellHandle,PChar(AWord));
    while wrds^<>nil do begin
      SuggestionList.Add(wrds^);
      inc(wrds);    // fixes the original statement "Inc(Integer(wrds), sizeOf(Pointer));"
      end;
    Result:=SuggestionList.Count;
    end;
  end;

// Add word to Hunspell dictionary
procedure TSynEditSpellCheck.AddDictWord (const AWord : String);
var
  sw : String;
begin
  sw:=Trim(AWord);
  if FEnabled and Assigned(FHunspellHandle) and (length(sw)>0) then with FUserDict do if IndexOf(sw)<0 then begin
    Add(sw);
    HunspellAdd(FHunspellHandle,PChar(sw));
    FModified:=true;
    if Assigned(FOnAddWord) then FOnAddWord(Self,AWord);
    end;
  end;

// Return true if the highlighter attribute points to section to be checked
function TSynEditSpellCheck.CheckHighlighterAttribute (const AttributeName : string) : boolean;
begin
  Result:=FCheckAttribs.IndexOf(AttributeName)>=0;
  end;

// Check the spelling of AWord
function TSynEditSpellCheck.CheckWord (const AWord : String): Boolean;
var
  sw : String;
begin
  sw:=Trim(AWord);
  if FEnabled and Assigned(FHunspellHandle) and (length(sw)>0) and
      not ((sscoIgnoreSingleChars in FOptions) and (Length(sw)=1)) then begin
    Result:=HunspellSpell(FHunspellHandle,PChar(sw))<>0;
    end
  else Result:=true;
  end;

// Check the whole text
function TSynEditSpellCheck.SpellCheck (AEditor : TSynEdit) : boolean;
var
  bAborted     : boolean;
  sToken,sWord : UnicodeString;
  pLastWord,
  pNextWord    : TBufferCoord;
  Attri        : TSynHighlighterAttributes;
  FCursor      : TCursor;
begin
  Result:=FEnabled;
  if Result then begin
    FBusy:=True;
    if Assigned(FOnStart) then FOnStart(Self);
    bAborted:=False;
    if sscoHourGlass in FOptions then begin
      FCursor:=Screen.Cursor;
      Screen.Cursor:=crHourGlass;
      end;
    with AEditor do begin
      if Trim(Lines.Text)='' then begin
        if sscoHourGlass in FOptions then Screen.Cursor:=FCursor;
        if Assigned(FOnDone) then FOnDone(Self);
        FBusy:=False;
        Exit;
        end;
      if not (sscoStartFromCursor in FOptions) then CaretXY:=BufferCoord(1, 1);
      if sscoHideCursor in FOptions then BeginUpdate;
      if sscoGoUp in FOptions then pNextWord:=PrevWordPosEx(CaretXY)
      else pNextWord:=NextWordPosEx(CaretXY);
      pLastWord:=pNextWord;
      while pNextWord.Char > 0 do begin
        Attri:=nil;
        // Check if the word is the last word, is cursor at end of text?
        if sscoGoUp in FOptions then begin
          if (PrevWordPosEx(CaretXY).Char=CaretX) and (Lines.Count=CaretY) then Break;
          end
        else begin
          if (NextWordPosEx(CaretXY).Char=CaretX) and (Lines.Count=CaretY) then Break;
          end;
        // Make sure we do not get any 'blank' words
        while length(Trim(GetWordAtRowCol(CaretXY)))=0 do begin
          { Just move to next word }
          if sscoGoUp in FOptions then pNextWord:=PrevWordPosEx(CaretXY)
          else pNextWord:=NextWordPosEx(CaretXY);
          CaretXY:=pNextWord;
          { If it the last word then exit loop }
          if pNextWord.Char=0 then Break;
          end;
        if pNextWord.Char=0 then Break;
        sWord:=GetWordAtRowCol(CaretXY);
        // Check if the word is in the dictionary
        if not assigned(Highlighter) then begin
          if not CheckWord(sWord) then Break;
          end
        else begin
          if not GetHighlighterAttriAtRowCol(CaretXY, sToken, Attri) then
            Attri:=Highlighter.WhitespaceAttribute;
          if Assigned(Attri) and (FCheckAttribs.IndexOf(Attri.Name)<>-1) and
            (not CheckWord(sWord)) then Break;
          end;
        // Prepare next word position
        if sscoGoUp in FOptions then pNextWord:=PrevWordPosEx(CaretXY)
        else pNextWord:=NextWordPosEx(CaretXY);
        CaretXY:=pNextWord;
      end;
      if sscoHideCursor in FOptions then EndUpdate;
      if sscoHourGlass in FOptions then Screen.Cursor:=FCursor;
    // Remove last word selection
      BlockBegin:=CaretXY;
      BlockEnd:=BlockBegin;
      end;
    if bAborted then begin
      if Assigned(FOnAbort) then FOnAbort(Self)
      end
    else if Assigned(FOnDone) then FOnDone(Self);
    FBusy:=False;
    end;
  end;

initialization
  LoadHunspellDll;
end.
