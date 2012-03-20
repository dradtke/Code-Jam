import java.io.*;
import java.util.*;

public class MinimumScalar {
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		int cases = in.nextInt();

		for (int i = 1; i<=cases; i++) {
			int length = in.nextInt();
			LinkedList<Integer> scalar1 = new LinkedList<Integer>();
			LinkedList<Integer> scalar2 = new LinkedList<Integer>();

			for (int j = 0; j<length; j++)
				scalar1.add(in.nextInt());

			for (int j = 0; j<length; j++)
				scalar2.add(in.nextInt());

			Collections.sort(scalar1);
			Collections.sort(scalar2);
			Collections.reverse(scalar2);

			int ans = calcScalar(scalar1, scalar2);
			System.out.println("Case #" + i + ": " + ans);
		}
	}

	static int calcScalar(List<Integer> scalar1, List<Integer> scalar2) {
		int result = 0;
		ListIterator<Integer> iter1 = scalar1.listIterator();
		ListIterator<Integer> iter2 = scalar2.listIterator();

		while (iter1.hasNext() && iter2.hasNext())
			result += iter1.next() * iter2.next();

		return result;
	}
}
