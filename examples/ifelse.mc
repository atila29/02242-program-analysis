{
    {int fst; int snd} R;
    int x;
    int[5] A;
    R := (1, -10);

    x := R.fst;

    if(x==1)
    {
        A[1] := 2;
    }
    else
    {
        A[1] := -2;
    }

    if(x>1)
    {
        A[2] := 2;
    }
    else
    {
        A[2] := -2;
    }

}