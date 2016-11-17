package in.blazingk.midifreqanalyzer;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import javax.sound.midi.MidiEvent;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.Sequence;
import javax.sound.midi.ShortMessage;
import javax.sound.midi.Track;

public class FreqAnalyzer {
	
	
	//The key for this integer is note1 * 1000 + note2
	public static HashMap<Integer, Integer> sequenceCount = new HashMap<Integer, Integer>();
	public static String[] filenames = {"song.mid", "whoyougonnacall.mid", "Misirlou.mid"};
	public static void main(String[] args) throws Exception{
		for (String s: filenames){
			analyzeSong(new File(s));
		}
		int sum = 0;
		for (int i = 0; i < sequenceCount.values().size(); i++){
			sum+= (Integer)sequenceCount.values().toArray()[i];
		}
		for (int i = 0; i < sequenceCount.keySet().size(); i++){
			int key = (Integer)sequenceCount.keySet().toArray()[i];
			int total = sequenceCount.get(key);
			System.out.println((key/1000)+"->"+(key%1000)+": "+((float)total/(float)sum));
		}
		
	}
	
	
	public static void analyzeSong(File f) throws Exception {
		Sequence sequence = MidiSystem.getSequence(f);
		for (Track track: sequence.getTracks()){
			ArrayList<Integer> noteValues = new ArrayList<Integer>();
			for (int i = 0; i < track.size(); i++){
				MidiEvent event = track.get(i);
				if (event.getMessage() instanceof ShortMessage){
					ShortMessage sm = (ShortMessage) event.getMessage();
					if (sm.getCommand() == 0x90){ //Note on
						noteValues.add(sm.getData1());
					}
				}
			}
			if (noteValues.size() > 1){
				for (int i = 1; i < noteValues.size(); i++){
					int lastValue = noteValues.get(i-1);
					int thisValue = noteValues.get(i);
					if (sequenceCount.containsKey((lastValue*1000)+thisValue)){
					sequenceCount.put((lastValue*1000)+thisValue, sequenceCount.get((lastValue*1000)+thisValue)+1);
					}else{
					sequenceCount.put((lastValue*1000)+thisValue, 1);	
					}
				}
			}
		}
	}
	
	
	

}


/*
   public static final int NOTE_ON = 0x90;
    public static final int NOTE_OFF = 0x80;
    public static final String[] NOTE_NAMES = {"C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"};

    public static void main(String[] args) throws Exception {
        Sequence sequence = MidiSystem.getSequence(new File("test.mid"));

        int trackNumber = 0;
        for (Track track :  sequence.getTracks()) {
            trackNumber++;
            System.out.println("Track " + trackNumber + ": size = " + track.size());
            System.out.println();
            for (int i=0; i < track.size(); i++) { 
                MidiEvent event = track.get(i);
                System.out.print("@" + event.getTick() + " ");
                MidiMessage message = event.getMessage();
                if (message instanceof ShortMessage) {
                    ShortMessage sm = (ShortMessage) message;
                    System.out.print("Channel: " + sm.getChannel() + " ");
                    if (sm.getCommand() == NOTE_ON) {
                        int key = sm.getData1();
                        int octave = (key / 12)-1;
                        int note = key % 12;
                        String noteName = NOTE_NAMES[note];
                        int velocity = sm.getData2();
                        System.out.println("Note on, " + noteName + octave + " key=" + key + " velocity: " + velocity);
                    } else if (sm.getCommand() == NOTE_OFF) {
                        int key = sm.getData1();
                        int octave = (key / 12)-1;
                        int note = key % 12;
                        String noteName = NOTE_NAMES[note];
                        int velocity = sm.getData2();
                        System.out.println("Note off, " + noteName + octave + " key=" + key + " velocity: " + velocity);
                    } else {
                        System.out.println("Command:" + sm.getCommand());
                    }
                } else {
                    System.out.println("Other message: " + message.getClass());
                }
            }

            System.out.println();
        }

    }
*/