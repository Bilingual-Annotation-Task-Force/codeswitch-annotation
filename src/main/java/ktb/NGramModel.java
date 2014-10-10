package ktb;

import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.*;

public class NGramModel { 
	
	public static void main (String[] args) {
		Map<String, Integer> model = createModel("abcd abcde bcde cdfg", 4);
		printModel(model);	
		getProbability("cde", model, 4);
	}
	
	public static Map<String, Integer> createModel(String input, int n){
		Map<String, Integer> model = new HashMap<String, Integer>();
		for (int k = -n + 1; k <= input.length() - n; k++){
			int start = (k < 0) ? 0 : k;
			int end = k + n;
			String ngram = input.substring(start, end);
			int freq = model.containsKey(ngram) ? model.get(ngram) : 0;
			model.put(ngram, freq + 1);
		}
		return model;
	}
	
	public static void printModel(Map<String, Integer> model){
		Set<String> ngrams = model.keySet();
		for (String n : ngrams){
			System.out.println("Ngram: " + n + ", freq: " + model.get(n));
		}
	}
	
	public static double getProbability(String input, Map<String, Integer> model, int n){
		
		double probability = 1;
		
		for (int k = 0; k <= input.length() - n; k++){
			// sequence of n characters
			String sequence = input.substring(k, k + n);
			System.out.println("Sequence: " + sequence);
			// sequence of n - 1 characters + metacharcter
			String x = sequence.substring(0, sequence.length() - 1) + ".";
			Pattern pattern = Pattern.compile(x);		
			
			//count of ngram occurrences that match the first n-1 characters of input sequence
			int countX = 0;
			//count of ngram occurrences that match input sequence
			int countSeq = 0;
			
			for (String ngram : model.keySet()){
				Matcher matcher = pattern.matcher(ngram);
				if (matcher.find()){
					String match = matcher.group();
					System.out.println("Match: " + match + ", ngram: " + ngram);
					countX += model.get(ngram);
					if (match.equals(sequence)){
						countSeq += model.get(ngram);
					}
				}
			}
			
			System.out.println("p: " + countSeq + " / " + countX);			
			probability *= ((double) countSeq / countX);
		}
		System.out.println("Probability: " + probability);
		return probability;
	}
	
}