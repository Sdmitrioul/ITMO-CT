/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author :Skroba Dmitri
 */
class Solution : MonotonicClock {
    private var first by RegularInt(0)
    private var second by RegularInt(0)
    private var third by RegularInt(0)

    private var firstReverse by RegularInt(0)
    private var secondRevers by RegularInt(0)

    override fun write(time: Time) {
        first = time.d1
        second = time.d2
        third = time.d3

        secondRevers = time.d2
        firstReverse = time.d1
    }

    override fun read(): Time {
        val firstReverseLocal = firstReverse
        val secondReverseLocal = secondRevers

        val thirdLocal = third
        val secondLocal = second
        val firstLocal = first

        val resSecond = if (firstReverseLocal == firstLocal) secondLocal else 0
        val resThird = if (firstReverseLocal == firstLocal && secondLocal == secondReverseLocal) thirdLocal else 0

        return Time(firstLocal, resSecond, resThird)
    }
}