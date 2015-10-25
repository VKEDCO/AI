package org.vkedco.parsing;

import java.util.Collection;
import java.util.List;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;

import opennlp.tools.sentdetect.SentenceDetectorME;
import opennlp.tools.sentdetect.SentenceModel;
import opennlp.tools.util.InvalidFormatException;
import edu.stanford.nlp.process.Tokenizer;
import edu.stanford.nlp.process.TokenizerFactory;
import edu.stanford.nlp.process.CoreLabelTokenFactory;
import edu.stanford.nlp.process.DocumentPreprocessor;
import edu.stanford.nlp.process.PTBTokenizer;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.HasWord;
import edu.stanford.nlp.ling.Sentence;
import edu.stanford.nlp.trees.*;
import edu.stanford.nlp.parser.lexparser.LexicalizedParser;

public class ParserDemo {

  
  // change these paths
  final static String OPEN_NL_BIN = "C:\\Users\\Vladimir\\Dropbox\\MyShare\\RESEARCH\\NarrativeMaps\\RouteData\\en-sent.bin";
  public static String route_01 = "Put the ATIA registration desk on your right side, and walk forward. In 25 feet, you will notice the Antigua hallway opening on the right side. Continue straight, and the perpendicular far wall of the Caribbean hallway is 75 feet ahead. Walk to the end of this hallway and turn left. The Grand Sierra Ballroom foyer begins 75 feet ahead as the carpet changes to tile. In another 35 feet, you reach the very center of this tiled foyer.";
  public static String small_route_01 = "Put the ATIA registration desk on your right side, and walk forward. In 25 feet, you will notice the Antigua hallway opening on the right side.";
  public static LexicalizedParser mLexParser = null;
  
  public static String[] baruch_college_route_01 = {
	  "You exit northbound bus 101, 102, or 103 at the 24th street stop and turn left.",
	  "Walk to 24th street in 80 feet.",
	  "This is a signalized, 2-lane crossing with 1-way Eastbound traffic only.",
	  "Cross 24th, and continue 225 feet to 25th street.",
	  "Cross 25th, a 2-lane signalized street with 1-way Westbound traffic.",
	  "Turn left and cross 3rd Ave.",
	  "This is a signalized, 6-lane crossing with 1-way Northbound traffic only.",
	  "Continue to the The Newman library, which is mid-block.",
	  "Follow the right side building edge.",
	  "This sidewalk is 15 feet wide, with left curbside obstacles such as poles, benches, trash, etc.",
	  "At 220 feet, a prominent right side landmark is the round metal revolving door of the library.",
	  "This is easily detected by a cane.",
	  "To the right of this door is the accessible entry door.",
	  "Enter here."
  };
  
  public static void main(String[] args) {
    mLexParser = LexicalizedParser.loadModel("edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz");
    //if (args.length > 0) {
    //  demoDP(lp, args[0]);
    //} else {
    //  demoAPI(lp);
    //}
    
    //demoAPI(mLexParser);
    for(String instruction: baruch_college_route_01) {
    	splitAndParseSentences(instruction);
    }
    //parseSentence(lp, route_01);
  }
  
  public static void splitAndParseSentences(String route) {
	  InputStream modelIn = null; 
      SentenceModel model = null;
      try {
    	  modelIn = new FileInputStream(OPEN_NL_BIN);
          model = new SentenceModel(modelIn);
          SentenceDetectorME sentenceDetector = new SentenceDetectorME(model);
          String sentences[] = sentenceDetector.sentDetect(route);
          for (int si = 0; si < sentences.length; si++) {
        	  System.out.println(sentences[si]);
        	  parseSentence(mLexParser, sentences[si]);
          }
      } catch (InvalidFormatException e) {
          e.printStackTrace();
      } catch (IOException e) {
          e.printStackTrace();
      }
  }
  
  public static void parseSentence(LexicalizedParser lp, String sent) {
	    String[] words = sent.split(" ");
	    // This option shows parsing a list of correctly tokenized words
	    List<CoreLabel> rawWords = Sentence.toCoreLabelList(words);
	    System.out.println(rawWords);

	    // This option shows loading and using an explicit tokenizer
	    TokenizerFactory<CoreLabel> tokenizerFactory =
	        PTBTokenizer.factory(new CoreLabelTokenFactory(), "");
	    Tokenizer<CoreLabel> tok =
	        tokenizerFactory.getTokenizer(new StringReader(sent));
	    List<CoreLabel> rawWords2 = tok.tokenize();
	    Tree parse = lp.apply(rawWords2);

	    TreebankLanguagePack tlp = new PennTreebankLanguagePack();
	    GrammaticalStructureFactory gsf = tlp.grammaticalStructureFactory();
	    GrammaticalStructure gs = gsf.newGrammaticalStructure(parse);
	    List<TypedDependency> tdl = gs.typedDependenciesCCprocessed();
	    Collection<TypedDependency> tdlcol = gs.typedDependencies();
	    Collection<TypedDependency> coltree = gs.typedDependenciesCollapsedTree();
	    System.out.println("Dependencies:");
	    System.out.println(tdl);
	    System.out.println(tdlcol);
	    System.out.println(coltree);
	    System.out.println();
	    // You can also use a TreePrint object to print trees and dependencies
	    TreePrint typedDepsCollapsed = new TreePrint("penn,typedDependenciesCollapsed");
	    typedDepsCollapsed.printTree(parse);
	    TreePrint typedDeps = new TreePrint("penn, typedDependencies");
	    typedDeps.printTree(parse);
	  }

  /**
   * demoDP demonstrates turning a file into tokens and then parse
   * trees.  Note that the trees are printed by calling pennPrint on
   * the Tree object.  It is also possible to pass a PrintWriter to
   * pennPrint if you want to capture the output.
   */
  public static void demoDP(LexicalizedParser lp, String filename) {
    // This option shows loading, sentence-segmenting and tokenizing
    // a file using DocumentPreprocessor.
    TreebankLanguagePack tlp = new PennTreebankLanguagePack();
    GrammaticalStructureFactory gsf = tlp.grammaticalStructureFactory();
    // You could also create a tokenizer here (as below) and pass it
    // to DocumentPreprocessor
    for (List<HasWord> sentence : new DocumentPreprocessor(filename)) {
      Tree parse = lp.apply(sentence);
      parse.pennPrint();
      System.out.println();

      GrammaticalStructure gs = gsf.newGrammaticalStructure(parse);
      Collection tdl = gs.typedDependenciesCCprocessed();
      System.out.println(tdl);
      System.out.println();
    }
  }

  /**
   * demoAPI demonstrates other ways of calling the parser with
   * already tokenized text, or in some cases, raw text that needs to
   * be tokenized as a single sentence.  Output is handled with a
   * TreePrint object.  Note that the options used when creating the
   * TreePrint can determine what results to print out.  Once again,
   * one can capture the output by passing a PrintWriter to
   * TreePrint.printTree.
   */
  public static void demoAPI(LexicalizedParser lp) {
    // This option shows parsing a list of correctly tokenized words
    String[] sent = { "This", "is", "an", "easy", "sentence", "." };
    List<CoreLabel> rawWords = Sentence.toCoreLabelList(sent);
    Tree parse = lp.apply(rawWords);
    parse.pennPrint();
    System.out.println();

    // This option shows loading and using an explicit tokenizer
    String sent2 = "This is another sentence.";
    TokenizerFactory<CoreLabel> tokenizerFactory =
        PTBTokenizer.factory(new CoreLabelTokenFactory(), "");
    Tokenizer<CoreLabel> tok =
        tokenizerFactory.getTokenizer(new StringReader(sent2));
    List<CoreLabel> rawWords2 = tok.tokenize();
    parse = lp.apply(rawWords2);

    TreebankLanguagePack tlp = new PennTreebankLanguagePack();
    GrammaticalStructureFactory gsf = tlp.grammaticalStructureFactory();
    GrammaticalStructure gs = gsf.newGrammaticalStructure(parse);
    List<TypedDependency> tdl = gs.typedDependenciesCCprocessed();
    System.out.println(tdl);
    System.out.println();

    // You can also use a TreePrint object to print trees and dependencies
    TreePrint tp = new TreePrint("penn,typedDependenciesCollapsed");
    tp.printTree(parse);
  }

  //private ParserDemo() {} // static methods only

}
