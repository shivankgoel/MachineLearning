import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;
import java.util.Map.Entry;
import java.util.Random;

public class NaiveBayes{
	
	public static void main(String[] args) {
		Double avgcorrectness=0.0;
		for(int j=0;j<=4;j++){
		List<Article>m=GetArticleFromText.GetListOfArticle(j);
		RecordStatistics r=GetStatistics.generateStatsFromArticles(m);
		NaiveBayesStats s=GetStatistics.getNaiveBayesStats(r);
		avgcorrectness+=Prediction.predict(j,s);
		}
		avgcorrectness/=5;
		System.out.println("Average Percentage Of Correctness Is: "+avgcorrectness+" %");
		System.out.println();
		System.out.println("If Predicted Randomly");
		System.out.println();
		avgcorrectness=0.0;
		for(int j=0;j<=4;j++){
			avgcorrectness+=Prediction.randompredict(j);
			}
			avgcorrectness/=5;
			System.out.println("Average Percentage Of Correctness Is: "+avgcorrectness+" %");
	}
}

class Article {

	public Map<String, Integer> noofeachword;
	public String newsgroup;

	public Article() {
		noofeachword = new HashMap<>();
	}

}

class GetArticleFromText {

	public static Article convertTextToArticle(String text) {
		String wordsarray[] = text.split("\\s+");
		Map<String, Integer> temp = new HashMap<>();
		Integer c;
		for (int i = 1; i < wordsarray.length; i++) {
			c = temp.get(wordsarray[i]);
			if (c == null)
				c = 0;
			temp.put(wordsarray[i], ++c);
		}
		Article a = new Article();
		a.noofeachword = temp;
		a.newsgroup = wordsarray[0];
		return a;
	}
	
	public static List<Article> GetListOfArticle(int j){
		List<Article> l=new ArrayList<>();
		for(int i=0;i<=4 ;i++){
		if(i!=j){
		try (BufferedReader br = new BufferedReader(new FileReader("E:\\This Sem\\con101\\con-assign3\\file_"+i+".txt"))) {
		    String line;
		    while ((line = br.readLine()) != null) {
		       Article a=GetArticleFromText.convertTextToArticle(line);
		       l.add(a);
		    }
		}
		catch(Exception e){
			System.out.println("Hey there is an exception "+e);
		}	
		}
		}
		return l;
	}
}

class RecordStatistics {
	int n; // record of total number of articles used
	Map<String, Integer> noofeachnewsgroup;
	Map<String, Map<String, Double>> noofword_newsgroupcombn; // how many times
																// the word
																// appear in
																// each of the
																// eight
																// newsgroup
		public RecordStatistics() {
		n = 0;
		noofeachnewsgroup = new HashMap<>();
		noofword_newsgroupcombn = new HashMap<>();
	}
}

class GetStatistics {

	public static RecordStatistics generateStatsFromArticles(List<Article> l){
		String newsgroup,word;
		Integer newsgroupcount;
		Double wordnewsgroupjointcount;
		RecordStatistics r=new RecordStatistics();
		Map<String, Double>newsgroupdistbnofword;
		for(Article a:l){
			++r.n; //increment no. of articles
			
			newsgroup=a.newsgroup; //increment no. of that newsgroup in which the article belongs
			newsgroupcount=r.noofeachnewsgroup.get(newsgroup);
			if(newsgroupcount==null)newsgroupcount=0;
			r.noofeachnewsgroup.put(newsgroup,++newsgroupcount);
			
			for(Map.Entry<String,Integer> x:a.noofeachword.entrySet()){ 
				//x iterates over noofeachword word by word
				word=x.getKey();
				newsgroupdistbnofword=r.noofword_newsgroupcombn.get(word);
				if(newsgroupdistbnofword==null){  //the word is encountered for the first time to the learner
					newsgroupdistbnofword=new HashMap<String,Double>();
					r.noofword_newsgroupcombn.put(word,newsgroupdistbnofword);
				}
				wordnewsgroupjointcount=r.noofword_newsgroupcombn.get(word).get(newsgroup);	
				if(wordnewsgroupjointcount==null)wordnewsgroupjointcount=0.0;
				r.noofword_newsgroupcombn.get(word).put(newsgroup,++wordnewsgroupjointcount);
			}
		}
		return r;
	}//end of generate stats from list of articles
	
	public static NaiveBayesStats getNaiveBayesStats(RecordStatistics r){
		NaiveBayesStats s=new NaiveBayesStats();
		s.t=r.n;
		s.nw=r.noofword_newsgroupcombn.size();
		s.ng=r.noofeachnewsgroup.size();
		
		String newsgroup;
		int c;
		for(Map.Entry<String,Integer>entry:r.noofeachnewsgroup.entrySet()){
			newsgroup=entry.getKey();
			c=entry.getValue(); //no of articles of that type
			s.logprobnewsgroup.put(newsgroup, Math.log((double)c)/s.t);//articles of that type/total articles
		}
		
		Map<String,Double>wordineachcateg=new HashMap<>(); //imp
		Double occ; //check if given newsgroup exist for given word
		Double wordofcateg=0.0; 
		for(String newsgrp:s.logprobnewsgroup.keySet()){
			wordofcateg=0.0;
			for(Map<String,Double>newsgroupcountforword:r.noofword_newsgroupcombn.values()){
				//we'll get list of hash maps with diff nwsgrps and we'll add all words of same nwsgrp
				occ=newsgroupcountforword.get(newsgrp);
				if(occ!=null){//if newsgroup exists in that hashmap ie for that word
					wordofcateg+=occ;
				}
			}
			wordineachcateg.put(newsgrp, wordofcateg);
		}
		
		Map<String,Double>distinctwordineachcateg=new HashMap<>(); //imp
		for(String newsgrp:s.logprobnewsgroup.keySet()){
			Double c1=0.0;
			for(String w:r.noofword_newsgroupcombn.keySet()){
				if(r.noofword_newsgroupcombn.get(w)!=null && r.noofword_newsgroupcombn.get(w).get(newsgrp)!=null){
					c1++;
				}
			}
			distinctwordineachcateg.put(newsgrp,c1);
		}
		
		String word;
		Map<String,Double>temp;
		Double count;
		Double loglikelihood;
		for(String nwsgrp:s.logprobnewsgroup.keySet()){
			for(Entry<String, Map<String, Double>>entry: r.noofword_newsgroupcombn.entrySet()){
				word=entry.getKey();
				temp=entry.getValue();
				count=temp.get(nwsgrp);
				if(count==null)count=0.0;
				loglikelihood=Math.log((count+1)/(wordineachcateg.get(nwsgrp)+distinctwordineachcateg.get(nwsgrp)));
				//no. of times that word appearinthat categ/(total word in that categ+totalwords in vocab)
				
				if(s.logprobwordgivennewsgroup.containsKey(word)==false){
					s.logprobwordgivennewsgroup.put(word, new HashMap<String,Double>());
				}
				s.logprobwordgivennewsgroup.get(word).put(nwsgrp,loglikelihood);
			}
			temp=null;
		}
	return s;	
	}
	
}//end of getstatistics class

class NaiveBayesStats{
	int t=0; //no. of training examples
	int ng=0; //no. of distinct newsgroup 
	int nw=0; //no. of distinct words
	Map<String,Double> logprobnewsgroup=new HashMap<>();
	Map<String,Map<String,Double>> logprobwordgivennewsgroup = new HashMap<>();
}

class Prediction{
	public static boolean predictarticle(Article a,NaiveBayesStats s){
		String newsgroup,word,predictedans="";
		Double lgprobnewsgrp,max=Double.NEGATIVE_INFINITY;
		Integer noofsuchwordinarticle;
		double lgprobdatagivencateg;
		for(Map.Entry<String, Double>e1:s.logprobnewsgroup.entrySet()){
			newsgroup=e1.getKey();
			lgprobnewsgrp=e1.getValue();
			lgprobdatagivencateg=0;
			for(Entry<String, Integer> e2:a.noofeachword.entrySet()){
				word=e2.getKey();
				noofsuchwordinarticle=e2.getValue();
				if(!s.logprobwordgivennewsgroup.containsKey(word)){
					continue;
				}
				lgprobdatagivencateg+=(noofsuchwordinarticle*s.logprobwordgivennewsgroup.get(word).get(newsgroup));
			}
			if((lgprobdatagivencateg+lgprobnewsgrp)>max){
				max=(lgprobdatagivencateg+lgprobnewsgrp);
				predictedans=newsgroup;
			}
		}
		if(predictedans.equals(a.newsgroup))return true;
		else return false;
		
	}
	
public static double predict(int j,NaiveBayesStats s){
	int totalarticles=0;
	int successfularticles=0;
	double successrate;
	try (BufferedReader br = new BufferedReader(new FileReader("E:\\This Sem\\con101\\con-assign3\\file_"+j+".txt"))) {
	    String line;
	    while ((line = br.readLine()) != null) {
	       Article a=GetArticleFromText.convertTextToArticle(line);
	       if(predictarticle(a,s)==true)successfularticles++;
	       totalarticles++;
	    }
	}
	catch(Exception e){
		System.out.println("Hey there is an exception "+e);
	}	
	successrate=((double)successfularticles/(double)totalarticles)*100;
	System.out.println("Case: "+j);
	System.out.println("Total Articles Predicted: "+totalarticles);
	System.out.println("Correctly Predicted: "+successfularticles);
	System.out.println("Percentage Of Correctness: "+successrate+" %");
	System.out.println();
	return successrate;
}

public static boolean predictarticlerandomly(Article a,int s){
	String predictedans="";
	switch(s){
	case 0:predictedans="rec.autos"; break;
	case 1:predictedans="rec.sport.hockey"; break;
	case 2:predictedans="talk.religion.misc"; break;
	case 3:predictedans="rec.motorcycles"; break;
	case 4:predictedans="talk.politics.guns"; break;
	case 5:predictedans="talk.politics.mideast"; break;
	case 6:predictedans="talk.politics.misc"; break;
	case 7:predictedans="rec.sport.baseball"; break;
		
	}
	if(predictedans.equals(a.newsgroup))return true;
	else return false;
	
}

public static double randompredict(int j){
int totalarticles=0;
int successfularticles=0;
double successrate;
try (BufferedReader br = new BufferedReader(new FileReader("E:\\This Sem\\con101\\con-assign3\\file_"+j+".txt"))) {
    String line;
    while ((line = br.readLine()) != null) {
       Article a=GetArticleFromText.convertTextToArticle(line);
       int s;
       s = (int)(Math.random()*8);
       if(predictarticlerandomly(a,s)==true)successfularticles++;
       totalarticles++;
    }
}
catch(Exception e){
	System.out.println("Hey there is an exception "+e);
}	
successrate=((double)successfularticles/(double)totalarticles)*100;
System.out.println("Case: "+j);
System.out.println("Total Articles Predicted: "+totalarticles);
System.out.println("Correctly Predicted: "+successfularticles);
System.out.println("Percentage Of Correctness: "+successrate+" %");
System.out.println();
return successrate;
}


}
