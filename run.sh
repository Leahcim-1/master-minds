stack build
cp /Users/xinhao/pfp-hw/project/.stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/master-minds-exe/master-minds-exe ./master-minds-exe
./master-minds-exe -f +RTS -M1024m -K1024m -N8 -ls -s -lf <<-EOF
10
4
8 7 6 5
EOF