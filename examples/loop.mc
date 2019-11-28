{
    {int fst; int snd} R;
    int x;
    int[5] A;
    R := (10, -10);

    x := R.fst;

    while(x > R.snd)
    {
        R.snd = R.snd + 1;
        x := x - 1;
    }
}