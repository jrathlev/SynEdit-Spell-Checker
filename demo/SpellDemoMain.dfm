object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Demo for use of SynEditSpell'
  ClientHeight = 435
  ClientWidth = 677
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcEditor: TPageControl
    Left = 0
    Top = 41
    Width = 677
    Height = 375
    ActivePage = tsHtml
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = pcEditorChange
    object tsHtml: TTabSheet
      Caption = 'HTML text:'
      ImageIndex = 1
      object SynEditHtml: TSynEdit
        Left = 0
        Top = 0
        Width = 669
        Height = 347
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = pmText
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.ShowLineNumbers = True
        Gutter.Width = 40
        Highlighter = SynHTMLSyn
        Lines.Strings = (
          '<h3>Die Funktionen in der '#220'bersicht:</h3>'
          '<ul>'
          '  <li>Anbenden von Laufwerken f'#252'r eine DOS-Anwendung:</li>'
          '  <ul>'
          
            '    <li>Beliebiges Verzeichnis auf dem Host-System als Festplatt' +
            'enlaufwerk</li>'
          
            '    <li>Beliebiges Host-DVD/CD-Laufwerk oder ISO-Image als CD-La' +
            'ufwerk</li>'
          '  </ul>'
          
            '  <li>Auswahl einer Anw'#228'ndung oder Batch-Datei mit opt. Paramete' +
            'rn, die'
          '    automatisch in der <em>DOSBox</em> gestartet wird</li>'
          
            '  <li>Eingabe von beliebigen DOS-Befehlen, die vor dem automatis' +
            'chen Start der'
          
            '    o.g. Anwendung ausgef'#252'hrt werden (z.B. ein Verzeichniswechse' +
            'l)</li>'
          
            '  <li>Auswahl eines Symbols f'#252'r die Anzeige in <strong>DosPanel<' +
            '/strong></li>'
          '  <li>Optionale Auswahl eines Keymappers</li>'
          
            '  <li>Auswahl einer Anleitung (z.B. Text oder PDF) f'#252'r die DOS-A' +
            'nwendung</li>'
          '  <li>Zus'#228'tzlicher Kommentar</li>'
          
            '  <li>Zus'#228'tzliche Optionen: Vollbildanzeige und automatisches Be' +
            'enden von '
          '    <em>DOSBox</em></li>'
          '  <li>Einstellung der Gr'#246#223'e des DOS-Arbeitsspeichers</li>'
          '  <li>Einstellung der Emulationsgeschwindigkeit</li>'
          '</ul>'
          ''
          '')
        Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoRightMouseMovesCursor, eoScrollHintFollows, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        WordWrap = True
        FontSmoothing = fsmNone
      end
    end
    object tsText: TTabSheet
      Caption = 'Plain text:'
      object SynEditText: TSynEdit
        Left = 0
        Top = 0
        Width = 669
        Height = 347
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = pmText
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.Visible = False
        Gutter.Width = 0
        Lines.Strings = (
          'MC-Tools Version 5'
          '=================='
          ''
          
            'MC-Tools is a peckage of several programs to develop software fo' +
            'r the 8051 family '
          
            'of microcontrollers (Siemens, Philips, Atmel, etc.). Ist consist' +
            's of:'
          ''
          '1. MC51: an Integrated Development Environment (IDE) '
          
            '  - Text editor with syntax highlighting to create Assembler and' +
            ' Pascal sources'
          
            '  - Integrated simmulator and debugger for all basic 8051 functi' +
            'ons including'
          '    timer and serrial interface'
          
            '  - Terminal window to communicate with a monitor program inside' +
            ' the'
          
            '    microcontroller and to download programs into the mamory of ' +
            'the'
          '    microcontroller using the Hex Intel format'
          
            '  - Module for flash programming Atmel AT89S8252/53 microcontrol' +
            'lers using the'
          '    serial interface'
          ''
          '2. McProjects: Projekt manager '
          ''
          
            '3. CheckIsp: Check the In-System-Programming of Atmel microcontr' +
            'ollers'
          ''
          
            '4. DiffIsp: Display differences between then IS-Programming and ' +
            'the source')
        Options = [eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoRightMouseMovesCursor, eoScrollHintFollows, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
        WordWrap = True
        FontSmoothing = fsmNone
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 416
    Width = 677
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 677
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object bbFont: TBitBtn
      Left = 5
      Top = 5
      Width = 36
      Height = 31
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        180000000000000300004E0000004E0000000000000000000000CC99FFCC99FF
        CC99FFCC99FFCC99FFCC99FFCC99FFAFAFAF6666662E2E2E2E2E2E2C2C2C2A2A
        2A626262AEAEAECC99FFCC99FFCC99FFCC99FFCC99FFCC99FFCC99FFCC99FFCC
        99FFA1A1A16666662E2E2E2B2B2B616161A0A0A0CC99FFCC99FFCC99FFCC99FF
        CC99FFCC99FFCC99FFCC99FFCC99FFCC99FFCC99FF8585853333333030307F7F
        7FCC99FFCC99FFCC99FFCC99FFCC99FFCC99FFCC99FFCC99FFCC99FFCC99FFCC
        99FFCC99FF8585853636363131317E7E7ECC99FFCC99FFCC99FFCC99FFCC99FF
        8282B16767B06B6BAF6969AD6565AA7E7EABCC99FF8585853939393434347E7E
        7ECC99FFCC99FFCC99FFCC99FFCC99FF9090B14E4EB12929B22626AE4A4AAA8D
        8DADCC99FF8686853C3C3C3737377F7F7FCC99FFCC99FFCC99FFCC99FFCC99FF
        CC99FF7D7DB13030B63030B37E7EAFCC99FFCC99FF8686864141413A3A3A7F7F
        7FCC99FFCC99FFCC99FFCC99FFCC99FFCC99FF7E7EB33232BA3232B78383B2CC
        99FFCC99FF8787874646463F3F3F808080CC99FFCC99FFCC99FFCC99FFCC99FF
        CC99FF7F7FB33535BE3232B9666692A9A9A8CC99FF8787874B4B4B4646468080
        80CC99FFA9A9A98F8F8FCC99FFCC99FFCC99FF7F7FB53939C22F2FB93535628C
        8C89CC99FF8989895050504A4A4A828282CC99FF868686404040CC99FFCC99FF
        CC99FF8080B63D3DC53131BC3B3B686565617F7F7F6A6A6A5050504A4A4A6161
        617777774A4A4A3B3B3BCC99FFCC99FFCC99FF8080B74242C93A3AC456568578
        78756D6D6C6D6D6A7171706F6F6F676767656565696969717171A6A6B4AFAFB3
        CC99FF8181B84747CD4444CA8181B4CC99FFADADAEA5A5AFCC99FFCC99FFCC99
        FFCC99FFCC99FFCC99FF6060C69797B8CC99FF8282B94B4BD04949CD8585B7CC
        99FF9393B35E5EB4CC99FFCC99FFCC99FFCC99FFCC99FFCC99FF5050D67575CB
        9696B87373BE5252D44F4FD17474BA9393B55F5FBB4343B9CC99FFCC99FFCC99
        FFCC99FFCC99FFCC99FF5454D34F4FD94C4CCE4848CC4A4AD14646CD4242C440
        40C03535C24C4CBACC99FFCC99FFCC99FFCC99FFCC99FFCC99FF}
      TabOrder = 0
      OnClick = bbFontClick
    end
  end
  object pmText: TPopupMenu
    OnPopup = pmTextPopup
    Left = 265
    Top = 5
    object pmiAddToDictionary: TMenuItem
      Caption = '&Add word to dictionary'
      ShortCut = 116
      OnClick = pmiAddToDictionaryClick
    end
    object pmiSuggestions: TMenuItem
      Caption = '&Suggested corrections'
    end
    object pmiMisspelling: TMenuItem
      Caption = '&Find next misspelling'
      ShortCut = 117
      OnClick = pmiMisspellingClick
    end
    object pmiSelectLanguage: TMenuItem
      Caption = 'Select &language'
      object pmiNoSpellcheck: TMenuItem
        Caption = 'None'
        GroupIndex = 82
        RadioItem = True
        OnClick = pmiNoSpellcheckClick
      end
    end
  end
  object SynHTMLSyn: TSynHTMLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 320
    Top = 10
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MinFontSize = 5
    MaxFontSize = 50
    Left = 395
    Top = 5
  end
end
