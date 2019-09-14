object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Demo for use of SynEditSpell'
  ClientHeight = 411
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
    Top = 0
    Width = 677
    Height = 392
    ActivePage = tsHtml
    Align = alClient
    TabOrder = 0
    OnChange = pcEditorChange
    ExplicitWidth = 548
    object tsHtml: TTabSheet
      Caption = 'HTML text:'
      ImageIndex = 1
      ExplicitWidth = 540
      object SynEditHtml: TSynEdit
        Left = 0
        Top = 0
        Width = 669
        Height = 364
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
        ExplicitWidth = 540
      end
    end
    object tsText: TTabSheet
      Caption = 'Plain text:'
      ExplicitWidth = 540
      object SynEditText: TSynEdit
        Left = 0
        Top = 0
        Width = 669
        Height = 364
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
    Top = 392
    Width = 677
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitWidth = 548
  end
  object pmText: TPopupMenu
    OnPopup = pmTextPopup
    Left = 40
    Top = 315
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
    Left = 90
    Top = 315
  end
end
