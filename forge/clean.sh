rm -rf compiled/
rm -rf server/compiled/
rm -rf lang/compiled/
rm -rf lang/alloy-syntax/compiled/
rm -rf core/compiled
rm -rf core/lang/compiled
rm -rf check-ex-spec/compiled
rm -rf check-ex-spec/lang/compiled
rm -rf check-ex-spec/library/compiled

# actually
rm -rf `find . -name compiled -type d -exec echo {} \;`
rm -rf `find . -name "*.bak" -type f -exec echo {} \;`

