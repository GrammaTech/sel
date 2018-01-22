package com.simple.multi.maven.app;

import com.simple.multi.maven.extras.SharedClassImpl;

import static java.lang.Integer.parseInt;

/**
 * Hello world!
 *
 */
public class App
{
    public static void main( String[] args )
    {
        int value = 50;
        if(args.length > 1) {
            value = Integer.parseInt(args[1]);
        } else {
            System.out.println("no arg :(");
        }


        String somthing = "This is something";

        SharedClassImpl.doLoopyThing(SharedClassImpl.getFinal());

        SharedClassImpl.setTest(value);
        SharedClassImpl.doLoopyThing(SharedClassImpl.getFinal());

        value -= 25;

        SharedClassImpl.doLoopyThing(value);

        System.out.println("Thats all folks");


    }
}
