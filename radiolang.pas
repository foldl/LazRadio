unit RadioLang;

{$mode objfpc}{$H+}

{
a LazRadio project is defined by a .lar file.

lazradio SYS-NAME;

var
  VAR-NAME: ModuleName;
  ...

begin

  // connect feature
  VAR-NAME :> VAR-NAME [ :> VAR-NAME];

  // connect data port 0
  VAR-NAME -> VAR-NAME [ -> VAR-NAME];

  // connect data port, soure port i, target port j
  VAR-NAME[I] -> VAR-NAME[J];

  // connect feature and data port 0
  VAR-NAME => VAR-NAME [ => VAR-NAME];

  // post a message
  VAR-NAME ! {M-ID, PRAMH, PRAML};
end.
}

interface

uses
  Classes, SysUtils;

implementation

end.

