import java.io.*;
import java.util.*;

public class ReverseWords {
	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		int cases = in.nextInt();
		in.nextLine();

		for (int i = 1; i<=cases; i++) {
			String line = in.nextLine();
			WordList list = new WordList();
			StringTokenizer tokenizer = new StringTokenizer(line);
			while (tokenizer.hasMoreTokens())
				list.addFirst(tokenizer.nextToken());

			System.out.println("Case #" + i + ": " + list.toString());
		}
	}

	static class WordList extends LinkedList<String> {
		public String toString() {
			StringBuilder builder = new StringBuilder();
			ListIterator<String> iter = this.listIterator();
			while (iter.hasNext()) {
				builder.append(iter.next());
				if (iter.hasNext())
					builder.append(" ");
			}

			return builder.toString();
		}
	}
}
