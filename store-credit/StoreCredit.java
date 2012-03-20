import java.io.*;
import java.util.*;

public class StoreCredit {
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		int cases = in.nextInt();

		for (int i = 1; i<=cases; i++) {
			int credit = in.nextInt();
			int n = in.nextInt();

			LinkedList<Integer> prices = new LinkedList<Integer>();
			for (int j = 0; j<n; j++)
				prices.add(in.nextInt());

			int index1 = -1;
			int index2 = -1;

			for (int j = 0; j<prices.size(); j++) {
				for (int k = j + 1; k<prices.size(); k++) {
					if (prices.get(j) + prices.get(k) == credit) {
						index1 = j + 1;
						index2 = k + 1;
						break;
					}
				}
			}

			System.out.println("Case #" + i + ": " + index1 + " " + index2);
		}
	}
}
