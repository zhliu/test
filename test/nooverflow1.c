int my_global_var=0;
void init_fun(int m){
	int y=1;
	my_global_var=y+m;
}
int  main(){
	int x=1;
	init_fun(x);
	while(x++);
	return 0;
}
//conkycolors
