package ktb;

import java.util.*;

public class NGramModel {
	
	private Map<String, Integer> model;
	private int n;
	
	public NGramModel(String input, int n){
		model = createModel(input, n);
		this.n = n;
	}
	
	public int getN(){
		return n;
	}
	
	public Map<String, Integer> getModel(){
		return model;
	}
	
	public String toString(){
		String str = "";
		for (String ngram : model.keySet()){
			str += "Ngram: " + ngram + ", freq: " + model.get(ngram) + "\n";
		}
		return str;
	}
	
	public static Map<String, Integer> createModel(String input, int n){
		Map<String, Integer> model = new HashMap<String, Integer>();
		for (int k = 0; k <= input.length() - n; k++){
			String ngram = input.substring(k, k + n);
			int freq = model.containsKey(ngram) ? model.get(ngram) : 0;
			model.put(ngram, freq + 1);
		}
		return model;
	}
	
	public double getProbability(String input){			
		double probability = 1;					
		for (int k = 0; k <= input.length() - n; k++){
			
			// frequency of n-gram
			String sequence = input.substring(k, k + n);
			int freq = model.containsKey(sequence) ? model.get(sequence) + 1 : 1;
			
			
			// frequency of (n-1)-gram + each possible character
			String subSequence = input.substring(k, k + n - 1);
			int subFreq = 0;		
			for (char i = 'a'; i <= 'z'; i++){
				String x = subSequence + i;
				subFreq += model.containsKey(x) ? model.get(x) + 1 : 1;
			}
			subFreq += model.containsKey(subSequence + ' ') ? model.get(subSequence + ' ') : 1;
			
			System.out.println("Count of [" + sequence + "]: " + freq + ", count of [" + subSequence + ".]: " + subFreq);
			probability *= (double) freq / subFreq;
		}
		return probability;
	}
	
}