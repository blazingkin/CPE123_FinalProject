package in.blazingk.midifreqanalyzer;

import java.util.HashMap;

public class Node {
	public int midivalue;
	public HashMap<Integer, Double> connection = new HashMap<Integer, Double>();

	public void normalizeNode(){
		double total = 0;
		for (int i = 0; i < connection.keySet().size(); i++){
			total += connection.get(connection.keySet().toArray()[i]);
		}
		for (int i = 0; i < connection.keySet().size(); i++){
			connection.put((int)connection.keySet().toArray()[i], connection.get(connection.keySet().toArray()[i])/total);
		}
	}
	
	public void print(){
		System.out.print("(make-markov-node "+midivalue+" (list ");
		for (int i = 0; i < 150; i++){
			if (connection.keySet().contains(i)){
				System.out.print(connection.get(i)+" ");
			}else if (FreqAnalyzer.allMidiNumbers.contains(i) && FreqAnalyzer.printZeros){
				System.out.print("0 ");
			}
		}
		System.out.println("))");
	}
}
