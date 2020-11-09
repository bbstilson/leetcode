object Solution {

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val map = nums.zipWithIndex.toMap
    val goodIdx = nums.indices.find { idx =>
      map.get(target - nums(idx)).isDefined &&
      map.get(target - nums(idx)).get != idx
    }.get

    Array(goodIdx, map(target - nums(goodIdx)))
  }
}
