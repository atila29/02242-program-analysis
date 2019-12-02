{
        {int fst; int snd} R;
        int x;
        int[5] A;
        x := 2*2;
        R := (1, -10);
        if (x == 4)
        {
            R.fst := x;
            x := R.snd;
        }
        else {
          R.snd := 15;
        }
        x := 5;
}