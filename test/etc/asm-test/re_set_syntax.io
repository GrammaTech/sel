Input data

%rsp     00 00 7f fd ef cc 4b f8
%rbp     00 00 7f fd ef cc 4c 30
%rdi     00 00 00 00 00 00 0b 06

00000000 0061a460   . . . . v v v v   00 00 00 00 00 00 00 00
00007ffd efcc4bd8   v v v v . . . .   00 00 7f fd 00 00 00 00
00007ffd efcc4be8   v v v v . . . .   00 00 00 00 00 00 00 00
00007ffd efcc4bf0   v v v v v v v v   00 00 00 00 00 00 00 00

Output data

%rsp     00 00 7f fd ef cc 4b f8
%rbp     00 00 7f fd ef cc 4c 30
%rdi     00 00 00 00 00 00 0b 06

00000000 0061a460   . . . . v v v v   00 00 00 00 00 00 0b 06
00007ffd efcc4bd8   v v v v . . . .   00 00 0b 06 00 00 00 00
00007ffd efcc4be8   v v v v . . . .   00 00 00 00 00 00 00 00
00007ffd efcc4bf0   v v v v v v v v   00 00 7f fd ef cc 4c 30
