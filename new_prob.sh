set -e

fixed=$(echo $2 | tr ' ' '_' | tr '[:upper:]' '[:lower:]')
new_path="./$1/$fixed"

mkdir $new_path
pushd . > /dev/null
cd $new_path

cat > README.md << EOF
# $2
EOF

cat > Solution.scala << EOF
object Solution {
  def main(args: Array[String]): Unit = {
    println("no data...")
  }
}
EOF

popd > /dev/null
echo "New files created in $new_path"
ls $new_path
echo ""
echo "Don't forget to update the Table of Contents!"
