package ktb;

import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.*;

import com.sun.xml.internal.fastinfoset.vocab.Vocabulary;


public class NGramModel {
	
	private Map<String, Integer> model;
	private int len;
	
	public NGramModel(String input, int n){
		model = createModel(input, n);
		len = n;
	}
	
	public int getLen(){
		return len;
	}
	
	public Map<String, Integer> getModel(){
		return model;
	}
	
	public String toString(){
		String str = "";
		Set<String> ngrams = model.keySet();
		for (String n : ngrams){
			str += "Ngram: " + n + ", freq: " + model.get(n) + "\n";
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
		
		for (int k = 0; k <= input.length() - len; k++){
			String sequence = input.substring(k, k + len);
			String subSequence = sequence.substring(0, sequence.length() - 1);
			//Pattern pattern = Pattern.compile(x);		
			
			// frequency of input sequence
			int countSeq = model.containsKey(sequence) ? model.get(sequence) : 1;		
			// frequency of first n-1 characters of input sequence + arbitrary character
			int countX = 0;		
			
/*			for (String ngram : model.keySet()){
				Matcher matcher = pattern.matcher(ngram);
				if (matcher.find()){
					countX += model.get(ngram);
				}
			}		*/
			
			for (char i = 'a'; i <= 'z'; i++){
				String x = subSequence + i;
				countX += model.containsKey(x) ? model.get(x) : 1;
			}
			countX += model.containsKey(subSequence + ' ') ? model.get(subSequence + ' ') : 1;
			
			//add 1 smoothing; 27 = vocabulary size (?) 
			//countSeq += 1;
			//countX += 27;
			
			System.out.println("Count of " + sequence + ": " + countSeq + ", count of " + subSequence + ".: " + countX);
			probability *= (double) countSeq / countX;
		}
		return probability;
	}
	
}