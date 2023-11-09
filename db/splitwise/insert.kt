import kotlin.math.exp

fun main() {
    val names =
        setOf("Dima", "Pavel", "Nastya", "Egor", "Artem", "Pasha", "Julya")
    val surnames =
        setOf("Skroba", "Demidova", "Yaroshevsky", "Yumanov", "Martunov")

    var user_counter = 0;
    println("Users")
    names.forEach { name ->
        surnames.forEach {
            println("($user_counter, '$name $it', '$name.$it@gnail.com'),")
            user_counter++;
        }
    }

    var groups_counter = 0;
    val way = setOf("поездка", "поход", "выезд");
    val what = setOf("на дачу", "в горы")

    println("Groups")
    way.forEach { o ->
        what.forEach {
            println("($groups_counter, '$o $it', 'Этот $o $it был прекрасным и незабываемым'),")
            groups_counter++;
        }
    }

    val groupsParticipants = mutableListOf<MutableSet<Int>>()

    println("Participants")
    for (i in 0 until groups_counter) {
        groupsParticipants.add(mutableSetOf());
        for (j in 0..(1..5).random()) {
            groupsParticipants[i].add((0 until user_counter).random())
        }

        groupsParticipants[i].forEach { println("($i, $it),") }
    }


    println("Group invitations")

    val groupsInvited = mutableListOf<MutableSet<Int>>()

    for (i in 0 until groups_counter) {
        groupsInvited.add(mutableSetOf());
        for (j in 0..(0..3).random()) {
            groupsInvited[i].add(
                (0 until user_counter).toSet().minus(groupsParticipants[i])
                    .random()
            )
        }

        groupsInvited[i].forEach { println("($i, ${groupsParticipants[i].random()}, $it),") }
    }

    println("payments")

    var paymentCounter = 0;

    for (i in 0..15) {
        val amount = (0..400).random()
        val groupId = (0 until groups_counter).random()
        val sender = groupsParticipants[groupId].random()
        val reciever = groupsParticipants[groupId].minus(sender).random()
        println("($paymentCounter, 'За то самое', $amount, $sender, $reciever, $groupId),")

        paymentCounter++
    }

    println("expenses")

    var expenseId = 0;

    val expensesNames = setOf("Вино и сыр", "Отель", "Билеты")

    val expenses = mutableListOf<Pair<Int, Int>>()

    for (i in 0..18) {
        val groupId = (0 until groups_counter).random()
        val payer =
            groupsParticipants[groupId].random()
        println("($expenseId, '${expensesNames.random()}', $groupId, $payer),")

        expenses.add(groupId to payer)

        expenseId++
    }


    println("Exp partition")
    for (i in 0 until  expenseId) {
        val (groupId, payer) = expenses[i]

        val part = mutableSetOf<Int>()
        for (j in (1 .. (1 .. groupsParticipants[groupId].size).random())) {
            val user = groupsParticipants[groupId].minus(payer).random()
            part.add(user)
        }

        part.forEach { user ->
            val amount = (100 .. 300).random()
            println("($user, $amount, $i),")
        }
    }
}