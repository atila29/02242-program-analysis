{
    {int fst; int snd} R;
    int x;
    int[5] A;
    R := (10, -10);

    x := 200;

    while(x > R.snd)
    {
        while(x > 100)
        {
            while(x > R.fst)
            {
                
                x := x - 1;
            }
        }
        R.snd := R.snd + 1;
    }
}