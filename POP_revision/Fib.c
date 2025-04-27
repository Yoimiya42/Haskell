
int fib_rec(int n)
{
    if (n <= 1)
        return n;

    return fib_rec(n - 2) + fib_rec(n - 1);
}

//-------------------------------
int tailFib(int n, int prev, int curr)
{
    if (n <= 1)
        return curr;

    return tailFib(n-1, curr, prev + curr);
}

int tailFib_aux(int n)
{
    if (n <= 1)
        return n;
    return tailFib(n, 0, 1);
}
//----------------------------------


#define MAX_LENGTH 100
int memo[MAX_LENGTH] = {-1};
 
int fib_memo_rec(int n)
{
    if(n <= 1)
        return n;

    if(memo[n] != -1)
        return memo[n];

    memo[n] = fib_memo_rec(n - 1) + fib_memo_rec(n - 2);
    return memo[n];
}    



//------------------------------------
int fib_iter(int n)
{
    int prev = 0;
    int curr = 1;
    int sum = 0;

    for(int i = 2; i <= n; i++)
    {
        sum = prev + curr;
        prev = curr;
        curr = sum;
    }

    return sum;
}