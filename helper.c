#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <ctype.h>

// return the max element of an integer array
int maximum(int arr[], int n)
{
    int i;
    int max = arr[0];
    for (i = 1; i < n; i++)
        if (arr[i] > max)
            max = arr[i];
    return max;
}

// return the min elem on an integer array
int minimum(int arr[], int n)
{
    int i;
    int min = arr[0];
    for (i = 1; i < n; i++)
        if (arr[i] < min)
            min = arr[i];
    return min;
}

// add all elems in an integer array
int sum(int arr[], int n)
{
    int i;
    int sum = 0;
    for (i = 0; i < n; i++)
        sum += arr[i];
    return sum;
}

// multiply all elems in an integer array
int product(int arr[], int n)
{
    int i;
    int result = 1;
    for (i = 0; i < n; i++)
        result *= arr[i];
    return result;
}

// reverse an array of integers
int* reverse(int arr[], int n) {
    int temp;
    int start = 0;
    int end = n - 1;
    while (start < end)
    {
        temp = arr[start];
        arr[start] = arr[end];
        arr[end] = temp;
        start++;
        end--;
    }
    return arr;
}

// return base^power
int power(int base, int power) {
  return pow(base, power);
}

// return absolute value of n
int absolute(int n) {
  return abs(n);
}

// print a boolean as true or false
int printboolean(bool b) {
  printf("%s", b ? "true\n" : "false\n");
  return 0;
}

 // substring of a string
char* substring(const char *str, int start, int end)
{
    int length = (int)strlen(str);
    if (length == 0) return "";
    else if (start > end) return "";
    else if (start >= length || end >= length) return "";

    char* substr = (char*)malloc(end - start + 1);

    int i = 0;
    int j = start;
    while (j < end && j < length) {
        substr[i] = str[j];
        i++;
        j++;
    }

    substr[i] = '\0';
    return substr;
}

int findchar(const char *str, const char ch)
{
    char *ptr;
    int index;
    ptr = strchr(str, ch);
    index = (int)(ptr - str); 
        
    return index; 
}

char* replaceletters(const char *str, const char ch, const char cha)
{
    int length = (int)strlen(str);
    char* s = (char*)malloc(length);

    int j =0;
    while(j < length)
    {
        s[j] = str[j];
	j++;
    }
     
    s[j] = '\0';

    int i=0;
    while(s[i]!='\0')
    {
        if(s[i]== ch)
        {
            s[i]= cha;
        }
        i++;
    }
    
    return s;
}

char* scrambleletters(const char *str)
{
    int length = (int)strlen(str);
    char* s = (char*)malloc(length);

    int j =0;
    while(j < length)
    {
        s[j] = str[rand() % (length-1)];
        j++;
    }      
    s[j] = '\0';
    
    return s;
}
// return the n-th fibonacci number
int fibonacci(int n)
{
	if (n <= 1)
		return n;
	return fibonacci(n - 1) + fibonacci(n - 2);
}

// return factorial of n
int factorial(int n) {
	if (n < 1) return 0;
	int i;
	int result = 1;
	for (i = 2; i <= n; i++) {
    	result *= i;
  	}
	return result;
}

/* print array of ints */
int printarray(int arr[], int n) {
	printf("[");
	int i;
	for (i = 0; i < n; i++) {
		if (i == 0)
			printf("%d", arr[i]);
		else
			printf(", %d", arr[i]);
	}
	printf("]\n");
	return 0;
}

// return the same string but upper case
char* touppercase(char *s) {
    int i = 0;
	int len = (int)strlen(s);
	char *str = (char*)malloc(len+1);
    while (s[i]) {
		str[i] = toupper(s[i]);
        i++;
    }
	str[len] = '\0';
    return str;
}

// return the same string but all lower case
char* tolowercase(char *s) {
    int i = 0;
	int len = (int)strlen(s);
	char *str = (char*)malloc(len+1);
    while (s[i]) { 
		str[i] = tolower(s[i]);
        i++; 
    }
	str[len] = '\0';
    return str; 
}

// returned the same string backwards
char* reversestring(char *str) {
	// Start here
	int length = (int)strlen(str);
	if (length == 0) return "";
	char *reversed = (char *)malloc(length+1);
	int i;
	for(i = 0; i < length; ++i) {
		reversed[length-i-1] = str[i];
	}
	reversed[length] = '\0';
	return reversed;
}

// return a random int
int randomnumber(int min, int max) {
	return (rand() % (max - min)) + min;
}

// return a random letter
char* randomletter() {
	char *s = (char *)malloc(2);
	char c = 'a' + randomnumber(0, 26);
	s[0] = c;
	s[1] = '\0';
	return s;
}

// Repeat the string n times and return it as one string
char* repeatstring(char *s, int n) {
	int length = (int)strlen(s);
	char *new_s = (char *)malloc(length*n+1);
	int i;
	int j = 0;
	for (i = 0; i < length*n; i++) {
		new_s[i] =  s[j%length];
		j++;
	}
	new_s[length*n] = '\0';
	return new_s;
}

// return the gcd of 2 ints
int greatestcommondenominator(int x, int y) {
    while(x != y) {
        if (x > y)
            x -= y;
        else
            y -= x;
    }
    return x;
}

// returns int array with numbers from min to max, and step allows you to print
// only numbers every `step` amount of numbers
int *rangewithstep(int min, int max, int step) {
	if (min < max && step > 0) {
		int length = (max - min) / step + 1;
		int *arr = (int *)malloc(length * sizeof(int));
		int i;
		for (i = 0; i < length; i++) {
			arr[i] = min;
			min += step;
		}
		return arr;
	} else if (max < min && step < 0) {
		int length = (min - max) / abs(step) + 1;
		int *arr = (int *)malloc(length * sizeof(int));
		int i;
		for (i = 0; i < length; i++) {
			arr[i] = min;
			min += step;
		}
		return arr;
	} else {
		return NULL;
	}
}

// returns int array with numbers from min to max
int *range(int min, int max) {
	return rangewithstep(min, max, 1);
}

// is an int prime
bool isprime(int n)
{
     if (n <= 1) return 0;
     if (n % 2 == 0 && n > 2) return 0;
	 int i;
     for(i = 3; i < n / 2; i += 2) {
         if (n % i == 0)
        	return false;
     }
     return true;
}

// Distance between 2 points
int distance(int x1, int y1, int x2, int y2) {
	return sqrt(pow(y2-y1, 2) + pow(x2-x1, 2));
}

char* concatenate(const char *str1, const char *str2)
{
	size_t len1 = strlen(str1);
	size_t len2 = strlen(str2);
	char* both = (char*)malloc(len1 + len2 + 1);

	strcpy(both, str1);
	strcat(both, str2);

	return both;
}



