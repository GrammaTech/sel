public class TestSimple_WhileForIfPrint {
    int field = 10;

    public static void main(String[] args) {
        int test1 = 0;
        while (test1 < 3) {
            System.out.println("In while loop " + test1);
            test1++;
        }
        int k=0;
        for (k = 0; k < 2; k++) {
            System.out.println("In for loop " + k);
            if (k == 1) {
                int test2 = 30;
                System.out.println("In for loop " + test2);
            }
        } if(k==2){
            float test3 = 10;
            System.out.println("In the last if" + test3);
        }
    }

    public String test1() {
        return "";
    }

    public int test3() {
        return 0;
    }

}
