// Preliminary Part about the 3n+1 conjecture
//============================================

object CW6a {

//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def collatz(n: Long) : Long = {
    if(n == 1) 0
    else if(n%2 == 0) collatz(n/2) + 1
    else collatz((n*3)+1) + 1
}

    /*
    if(n == 1){
        0
    }
    else if(n%2 == 0){
        //divide by 2
        collatz(n/2) + 1
    }
    else{
        //multiply by 3 and add 1
        collatz((n*3)+1) + 1
    }
    */


//(2) Complete the collatz_max function below. It should
//    calculate how many steps are needed for each number 
//    from 1 up to a bound and then calculate the maximum number of
//    steps and the corresponding number that needs that many 
//    steps. Again, you should expect bounds in the range of 1
//    up to 1 Million. The first component of the pair is
//    the maximum number of steps and the second is the 
//    corresponding number.

def collatz_max(bnd: Long) : (Long, Long) = {
    val numbers = for (n <- 1L to bnd) yield collatz(n)
    val steps = numbers.max
    (steps, numbers.indexOf(steps)+1) 
    /*val numbers: List[(Long, Long)] = for (n <- 1L to bnd) yield (collatz(n),n)
    numbers.sortBy(._1)
    numbers.head*/
}

//collatz_max(10)

//(3) Implement a function that calculates the last_odd
//    number in a collatz series.  For this implement an
//    is_pow_of_two function which tests whether a number 
//    is a power of two. The function is_hard calculates 
//    whether 3n + 1 is a power of two. Again you can
//    assume the input ranges between 1 and 1 Million,
//    and also assume that the input of last_odd will not 
//    be a power of 2.

def is_pow_of_two(n: Long) : Boolean = {
    if ((n & (n-1)) == 0) true
    else false
}

//is_pow_of_two(12)

def is_hard(n: Long) : Boolean = {
    is_pow_of_two((3*n)+1)
}

def last_odd(n: Long) : Long = {

    if(n%2 == 0) last_odd(n/2) 
    else {
        if(is_hard(n) == true) n
        else last_odd((n*3)+1)
    }

}
//last_odd(84)

}

