(* Delphi interface to HunSpellX86.dll
   ===================================

   © Dr. J. Rathlev, D-24222 Schwentinental (kontakt(a)rathlev-home.de)

   The contents of this file may be used under the terms of the
   Mozilla Public License ("MPL") or
   GNU Lesser General Public License Version 2 or later (the "LGPL")

   Software distributed under this License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   based on NHunspell by Thomas Maierhofer
   http://nhunspell.sourceforge.net

   Vers. 1.0 - August 2019
   last modified: June 2022
   *)

unit HunspellLib;

interface

uses System.Sysutils, Winapi.Windows;

const
{$IFDEF WIN32}
  HunspellDLLName = 'HunspellX86.dll';
{$ELSE}
  HunspellDLLName = 'HunspellX64.dll';
{$ENDIF}

type
  HyphenResult = record
    hyphenatedWord: PWideChar;
    hyphenationPoints: PInt;
    repData: ^PWideChar;
    posData: PInt;
    cutData: PInt;
  end;

  PHyphenResult = ^HyphenResult;

 var
  HunspellInit: function(const AffixBuffer: PAnsiChar; AffixBufferSize: Integer; const DictionaryBuffer: PAnsiChar; DictionaryBufferSize: Integer; key: PAnsiChar): Pointer; cdecl;
  HunspellFree: procedure(Handle: Pointer); cdecl;
  HunspellSpell: function(Handle: Pointer; const Word: PWideChar): Integer; cdecl;
  HunspellSuggest: function(Handle: Pointer; const Word: PWideChar): Pointer; cdecl;
  HunspellAnalyze: function(Handle: Pointer; const Word: PWideChar): Pointer; cdecl;
  HunspellStem: function(Handle: Pointer; const Word: PWideChar): Pointer; cdecl;
  HunspellGenerate: function(Handle: Pointer; const Word: PWideChar; const SampleWord: PWideChar): Pointer; cdecl;
  HunspellAdd: function(Handle: Pointer; const Word: PWideChar): Integer; cdecl;
  HunspellAddWithAffix: function(Handle: Pointer; const Word, Affix: PWideChar): Integer; cdecl;
  HyphenInit: function(const DictionaryBuffer: PAnsiChar; DictionaryBufferSize: Integer): Pointer; cdecl;
  HyphenFree: procedure(Handle: Pointer); cdecl;
  HyphenHyphenate: function(Handle: Pointer; const Word: PWideChar): PHyphenResult; cdecl;

function LoadHunspellDll : Boolean;

implementation

var
  HunspellDll: HMODULE;

function LoadHunspellDll : Boolean;
begin
  Result := False;
  if HunspellDll = 0 then begin
    HunspellDll := SafeLoadLibrary(HunspellDLLName);
    if HunspellDll <> 0 then begin
      @HunspellInit := GetProcAddress(HunspellDll, 'HunspellInit');
      if not Assigned(HunspellInit) then Exit;
      @HunspellFree := GetProcAddress(HunspellDll, 'HunspellFree');
      if not Assigned(HunspellFree) then Exit;
      @HunspellSpell := GetProcAddress(HunspellDll, 'HunspellSpell');
      if not Assigned(HunspellSpell) then Exit;
      @HunspellSuggest := GetProcAddress(HunspellDll, 'HunspellSuggest');
      if not Assigned(HunspellSuggest) then Exit;
      @HunspellAnalyze := GetProcAddress(HunspellDll, 'HunspellAnalyze');
      if not Assigned(HunspellAnalyze) then Exit;
      @HunspellStem := GetProcAddress(HunspellDll, 'HunspellStem');
      if not Assigned(HunspellStem) then Exit;
      @HunspellGenerate := GetProcAddress(HunspellDll, 'HunspellGenerate');
      if not Assigned(HunspellGenerate) then Exit;
      @HunspellAdd := GetProcAddress(HunspellDll, 'HunspellAdd');
      if not Assigned(HunspellAdd) then Exit;
      @HunspellAddWithAffix := GetProcAddress(HunspellDll, 'HunspellAddWithAffix');
      if not Assigned(HunspellAddWithAffix) then Exit;

      @HyphenInit := GetProcAddress(HunspellDll, 'HyphenInit');
      if not Assigned(HyphenInit) then Exit;
      @HyphenFree := GetProcAddress(HunspellDll, 'HyphenFree');
      if not Assigned(HyphenFree) then Exit;
      @HyphenHyphenate := GetProcAddress(HunspellDll, 'HyphenHyphenate');
      if not Assigned(HyphenHyphenate) then Exit;
      end;
    end;
  Result := HunspellDll <> 0;
  end;

initialization
  HunspellDll:=0;

finalization
  if HunspellDll<>0 then FreeLibrary(HunspellDll);

end.
