package com.simple.multi.maven.extras;

import java.util.Random;

public class SharedClassImpl {
    private static Integer test = 100;

    private static Random rand = new Random();

    public static final Integer getFinal() {
        return test;
    }

    public static void doLoopyThing(int rounds) {
        int i = 0;
        int p = 100;
        int incrementer = 1;

        if(p < rounds) {
            System.out.println("I must say I expected more. lets rais it");
            rounds *= p;
        }

        int oldCounter = i;
        for (; i < rounds; i += incrementer ) {
            goingUp();
        }
        do {
            goingDown();
            i -= incrementer;
        } while(i >= 0);

    }

    private static void goingUp () {
        int n = rand.nextInt(50) + 1;
        System.out.println("going up: rands: '" + n + "'");
    }

    private static void goingDown () {
        int n = rand.nextInt(60) + 1;

        System.out.println("goind down: rands '" + n + "'");
    }

    public static void setTest(int t) {
        test = t;
    }
}
