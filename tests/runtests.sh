#!/bin/bash
# Temporarily switch the active domain to be the test domain. After running the
# tests switch it back.
ln -s "../Beagle" Beagle
mv ../Beagle/Domain.hs ../Beagle/Domain.hs.bak
ln -s "../Beagle/Domain/Test.hs" ../Beagle/Domain.hs
runghc Tests.hs
rm ../Beagle/Domain.hs
mv ../Beagle/Domain.hs.bak ../Beagle/Domain.hs
rm Beagle
