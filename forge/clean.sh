rm -rf compiled/
rm -rf server/compiled/
rm -rf lang/compiled/
rm -rf lang/alloy-syntax/compiled/
rm -rf core/compiled
rm -rf core/lang/compiled
rm -rf check-ex-spec/compiled
rm -rf check-ex-spec/lang/compiled
rm -rf check-ex-spec/library/compiled

# actually, just search for patterns at arbitrary depth
rm -rf `find . -name compiled -type d -exec echo {} \;`
rm -rf `find . -name "*.bak" -type f -exec echo {} \;`
rm -rf `find . -name "*.rkt~" -type f -exec echo {} \;`
rm -rf `find . -name "*.frg~" -type f -exec echo {} \;`
rm -rf `find . -name "*.smt2~" -type f -exec echo {} \;`
