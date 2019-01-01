package test;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;
public class GeneticAlgorithm {
	
	public static class tup<A,B>{
		public A a;
		public B b;
		public tup(A a, B b){
			this.a = a;
			this.b = b;
		}
	}
	
	public static class Tuple<A,B> implements Comparable<Tuple<A,B>>, Comparator<Tuple<A,B>>{
		public tup<A,B> t;
		public Tuple(){};
		public Tuple(A a, B b){
			t = new tup<A,B>(a,b);
		}
		@Override
		public int compareTo(Tuple<A, B> o) {
			return ((Double) t.a).compareTo((Double) o.t.a);
		}
		@Override
		public int compare(Tuple<A, B> left, Tuple<A, B> right) {
			// TODO Auto-generated method stub
			return Double.compare( (Double)left.t.a, (Double) right.t.a );
		}
		
	}
	public static class Range<T> {
	    private T low;
	    private T high;
	    private Function<T, T> iter_action;

	    public Range(T low, T high, Function<T, T> iter_action){
	        this.low         = low;
	        this.high 		 = high;
	        this.iter_action = iter_action;
	    }

	    public void forEach(Consumer<T> f){
	    	while (!low.equals(high) ){
	    		f.accept(low);
	    		low = iter_action.apply(low);
	    	}
	    }
	}

	
	@SuppressWarnings("serial")
	public static class Seq<T> extends ArrayList<T>{
		
		Seq(int capi){
			super(capi);
		}	
		
		public <R> Seq<R> Do( Function<Seq<T>,Seq<R>> f){	
			return f.apply(this);
				
		}
		
		public  Integer[] argsort(){
			int size = this.size();
			Integer[] ret= new Integer[size];
			Tuple<T, Integer> comp = new Tuple<T, Integer>();
			Seq<Tuple<T, Integer>> idx_arr =  this.MapIndexed((i,item)->  new Tuple<T, Integer>(item, i) );
			idx_arr.sort(comp);
			return idx_arr.Map(tup->tup.t.b).toArray(ret);			    
	 
	    }
		
		public <R> Integer[] argsort_by(Function<T,R> f){
			return this.Map(f).argsort();
	    }
		public <I, R> Integer[] argsort_by(BiFunction<I, T, R> f, I msg){
			return this.Map(f, msg).argsort(); 
	    }	
		public <R> Seq<R> Map(Function<T, R> f){
			int size = this.size();
			Seq<R> newlist = new Seq<R>(size);
			new Range<Integer>(0, size, i->++i).forEach(i->newlist.add(f.apply(this.get(i))));
			return newlist;
		}
		public <R> Seq<R> MapIndexed(BiFunction<Integer, T, R> f){
			int size = this.size();
			Seq<R> newlist = new Seq<R>(size);
			new Range<Integer>(0, size, i->++i).forEach(i->newlist.add(f.apply(i,this.get(i))));
			return newlist;
		}
		public <I, R> Seq<R> Map(BiFunction<I, T, R> f, I msg){
			int size = this.size();
			Seq<R> newlist = new Seq<R>(size);
			new Range<Integer>(0, size, i->++i).forEach(i->newlist.add(f.apply(msg, this.get(i))));
			return newlist;
		}
		public  Double Mean( Function<T, Double> f){		
			Double sum = 0.0;
			for(double item :this.Map(f)) sum+=item;
			return sum/this.size();
		}

		public Seq<Double> Diff(){		
			Seq<Double> newlist = new Seq<Double>(this.size()-1);
			new Range<Integer>(1, this.size(), i->++i).forEach( i-> newlist.add( (Double)this.get(i)-(Double)this.get(i-1) ));
			return newlist;
		}
		public Seq<T>  Indexed(Integer[] indices){
			int size= indices.length;
			Seq<T> newlist = new Seq<T>(size);
			new Range<Integer>(0, size, i->++i).forEach( i -> newlist.add(this.get(indices[i])));
			return newlist;
		}
		
		@SuppressWarnings("unchecked")
		public Seq<T>  Index(Integer[] indices){
			int size= indices.length;		
			Object[] swap = new Object[size];
			new Range<Integer>(0, size, i->++i).forEach( i ->swap[i] = this.get(indices[i]));
			for(int i = 0; i < size; ++i)
				this.set(i, (T) swap[i]);
			return this;
		}
		
		
		public Seq<T> forEachIndexed(BiConsumer<Integer, T> f){
			new Range<Integer>(0, this.size(), i->++i).forEach( i->f.accept(i, this.get(i)));
			return this;
		}
		public Seq<T> forEachAsIndex(Consumer<Integer> f){
			new Range<Integer>(0, this.size(), i->++i).forEach(i->f.accept(i));
			return this;
		}
		public Seq<T> forEachWithIndex(BiConsumer<Integer, Seq<T>> f){
			new Range<Integer>(0, this.size(), i->++i).forEach(i->f.accept(i, this));
			return this;
		}
		public String Join(String spliter){
			return String.join(spliter, this.Map(i->(String)i ));
		}
		
		public void reload(int n){
			new Range<Integer>(n, this.size(), i->++i).forEach(i->this.set(i, this.get(i%n)));
		}
		
		public void reload(int n, int stop){
			new Range<Integer>(n, stop, i->++i).forEach(i->this.set(i, this.get(i%n)));
		}
		public void load(int begin , Seq<T> toload){
			new Range<Integer>(0, toload.size(), i->++i).forEach(i->this.set(i+begin, toload.get(i)));
		}
		public void load(int begin ,T[] toload){
			new Range<Integer>(0, toload.length, i->++i).forEach(i->this.set(i+begin, toload[i]));
		}
		public void loadBy(int begin , Function<T, T>  ftoload){
			new Range<Integer>(begin, this.size(), i->++i).forEach(i->this.set(i, ftoload.apply(this.get(i))));
		}
		public void loadBy(int begin , int stop, Function<T, T>  ftoload){
			new Range<Integer>(begin, stop, i->++i).forEach(i->this.set(i, ftoload.apply(this.get(i))));
		}


		
		public static <T> Seq<T> fromList( List<T> arrlist ){
			Seq<T> seq = new Seq<T>(arrlist.size());
			arrlist.forEach(item -> seq.add(item));
			return seq;
		}
		public static <T> Seq<T> fromArray( T[] arr ){
			Seq<T> seq = new Seq<T>(arr.length);
			for(T item : arr ) seq.add(item); 
			return seq;
		}
		
	}
	
	Random rnd   = new Random();
	char [] tmp_chr_arr = null; // to init : new char[length]
	char[] chro1 = null;
	char[] chro2 = null;

	int remain_num = 20; // to init 
	int iterations = 0;  // to init 
	
	int length  = 0;   // to init : length
	int[][] statas = null;
	double prob_sep = 0.0; // to init : 1.0 / length
	
	double p_m = 0.02;// to init 
	double p_c = 0.6;// to init 
	int popu_size = 500;
	Seq<char[]> popu   = null; // to init : size
	Seq<Double> scores = null;
	
	
	public GeneticAlgorithm(){};
	public char[] generate(){
		int tmp_i   = 0;
		char[] ret = new char[length];
		for(tmp_i=0; tmp_i< length; ++tmp_i)
			ret[tmp_i] = rnd.nextBoolean()?'1' : '0';
		return ret;
	}
	public char[] mutate(char[] chromosome){
		int tmp_i   = 0;
		for(tmp_i=0; tmp_i<chromosome.length; ++tmp_i)
			if(rnd.nextDouble() > p_m) chromosome[tmp_i] = chromosome[tmp_i] == '0' ? '1' : '0'; 
		return chromosome;
	}
	public void crossover(char[] chromosome1, char[] chromosome2, double prob_sep){
		int tmp_i   = 0;
		int split_idx =  (int) (rnd.nextDouble()/prob_sep);
		for(tmp_i=0; tmp_i< split_idx; ++tmp_i){
			tmp_chr_arr[tmp_i] = chromosome1[tmp_i];
			chromosome1[tmp_i] = chromosome2[tmp_i];
			chromosome2[tmp_i] = tmp_chr_arr[tmp_i];
		} 
	}
	public void evolution(ToDoubleFunction<String> fitness){
		int j,i;
		scores = popu.Map(arr -> - fitness.applyAsDouble(String.valueOf(arr)));
		Integer[] indices  = scores.argsort();
		popu  = popu.Index(indices);
		char [] test;
		for(i = 0; i < remain_num;++i){
			test = popu.get(i);
			for(j = 0 ;j< length; ++j)
			{
				if (test[j] == '0')
					this.statas[j][0]+=1;
				else
					this.statas[j][1]+=1;
			}
		}
		popu.forEachIndexed( (idx,chr_arr) ->
		{	if(idx>5)
				mutate(chr_arr);
		} );
		int pow = remain_num*remain_num*remain_num;
		
		
		for(i = remain_num, j = remain_num; i<2*remain_num; ++i, j+=2 ){
			chro1 = popu.get(remain_num - (int) Math.pow(1.0*Math.abs(rnd.nextInt())%(pow), 0.33)).clone();
			chro2 = popu.get(remain_num - (int) Math.pow(1.0*Math.abs(rnd.nextInt())%(pow), 0.33)).clone();
			popu.set(j  , chro1);
			popu.set(j+1, chro2);
			if (rnd.nextDouble()>p_c)
				crossover(popu.get(j), popu.get(j+1), prob_sep);
		}
		
		for(; j<popu_size; ++j)
			popu.set(j, generate());
		
		
		

	}
	public String run(ToDoubleFunction<String> fitness, int length, double p_c, double p_m) {
		return run(fitness, length, p_c, p_m, 1000);
	}
	public String run(ToDoubleFunction<String> fitness, int length, double p_c, double p_m,int iterations) {
		this.statas = new int[length][2]; 
		this.iterations = iterations;
		this.p_c = p_c;
		this.p_m = p_m;
		this.length     = length;
		this.popu_size  = 1000; 
		this.popu       = new Seq<char[]>(this.popu_size);
		int i;
		for(i = 0; i<this.popu_size; ++i)
			this.popu.add(generate());
		this.prob_sep   = 1.0 / length;
		this.remain_num  =  this.popu_size/5;
		this.tmp_chr_arr = new char[length];
		for (i = 0; i < this.iterations; ++i){
			evolution(fitness);
		}
		char [] ret = new char[length];
		for(i=0; i<length; ++i)
			ret[i] = this.statas[i][0]>this.statas[i][1]?'0':'1';
		return String.valueOf(ret);
	}
	
	public static void main(String[] args){
		ToDoubleFunction<String> f = (String s)-> 
		((s.charAt(0)=='1')?1:0) +((s.charAt(1)=='1')?1:0)+ ((s.charAt(2)=='0')?1:0)+((s.charAt(3)=='0')?1:0)
		+((s.charAt(4)=='0')?1:0)+((s.charAt(5)=='0')?1:0)+((s.charAt(6)=='0')?1:0)+((s.charAt(7)=='0')?1:0)
		+((s.charAt(8)=='0')?1:0)+((s.charAt(9)=='0')?1:0)+((s.charAt(10)=='1')?1:0)+((s.charAt(11)=='1')?1:0)
		;
		GeneticAlgorithm g = new GeneticAlgorithm();
		System.out.println(g.run(f, 12, 0.6, 0.002));
		

		
	}
	
	
}
