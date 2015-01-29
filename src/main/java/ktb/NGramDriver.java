package ktb;

import java.io.*;
import java.util.Scanner;

public class NGramDriver{
	
	public static void main (String[] args) {
		Scanner scan = new Scanner(System.in);
		System.out.print("Enter training file: ");
		String fileName = scan.next();
		scan.nextLine();
		System.out.print("Enter size of n: ");
		int n = scan.nextInt();
		scan.nextLine();
		System.out.print("Enter sentence: ");
		String sentence = scan.nextLine();
		String input = "";
		
		// Remove non-characters/non-spaces from input file, convert to lowercase
		try {
			FileReader reader = new FileReader(fileName);
			int next = reader.read();
			while (next != -1){
				char c = (char) next;
				if (c >= 'A' && c <= 'Z')
					c += 32;
				if ((c >= 'a' && c <= 'z') || c == 32) // 32 = ' '
					input += c;
				next = reader.read();
			}
			reader.close();
		}
		
		catch (IOException e) {
			System.out.println("error: " + e.getMessage());
		}	

		NGramModel NGM = new NGramModel(input, n);
		//System.out.println("Model: " + NGM);	
		double probability = NGM.getProbability(sentence);
		System.out.println("Probability: " + probability);
	}
	
}
