# Leetcode

Collection of my solutions to [LeetCode problems](https://leetcode.com/bbstilson/).

## Running

All solutions are written in Scala and are run as scripts using [Ammonite](http://ammonite.io).

```bash
amm difficulty/some_problem/solution.sc
```

For example:

```bash
amm medium/valid_sudoku/solution.sc
true
false
```

## To create a new problem template

```bash
./new_problem.sh <difficulty> "Problem Name"
```

For example:

```bash
./new_prob.sh medium "Some Medium Thing"
New files created: ./medium/some_medium_thing
README.md Solution.scala

Don't forget to update the Table of Contents!
```

## Table of Contents

### Easy

[Two Sum](./easy/two_sum)

[Longest Common Prefix](./easy/longest_common_prefix)

[Merge Two Sorted Lists](./easy/merge_two_sorted_lists)

[Palindrome Number](./easy/palindrome_number)

[reverse Integer](./easy/reverse_integer)

### Medium

[Valid Sudoku](./medium/valid_sudoku)

[Combination Sum](./medium/combination_sum)

[Container With Most Water](./medium/container_with_most_water)

[Open the Lock](./medium/open_the_lock)

### Hard

### Other

[Treasure Island](./other/treasure_island)
