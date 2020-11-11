set -e

fixed=$(echo $2 | tr ' ' '_' | tr '[:upper:]' '[:lower:]')
new_path="./$1/$fixed"

mkdir $new_path
pushd . > /dev/null
cd $new_path

cat > README.md << EOF
# $2
EOF

touch solution.sc

popd > /dev/null
echo "New files created in $new_path"
ls $new_path
echo ""
echo "Don't forget to update the Table of Contents!"
echo "[$2]($new_path)"
