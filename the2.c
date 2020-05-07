#include <stdio.h>
#include <stdlib.h>
#include "math.h"
#include "the2.h"


/*
INPUT:
    int *row_count: vertical size of the matrix to be assigned (passed by reference from the main function)
    int *row_size: horizontal size of the matrix to be assigned (passed by reference from the main function)

OUTPUT:
    double **matrix: created data matrix

METHOD:
    This function creates the matrix streamed via stdin. 
    During this process, assigns row_count and row_size variables.
*/
double **initialize_the_data(int *row_count, int *row_size) {
    int m;
    double temp;
    double **ans;
    
    
    ans = (double**) malloc(1*sizeof(double*));
    ans[0] = (double*) malloc(1*sizeof(double));
    
    for(*row_size=0;*row_size<21;(*row_size)++){
        ans[0] = realloc(ans[0],(*row_size+1)*sizeof(double));
        scanf("%lf",&temp);
        if(temp==-1){
            break;
        }
        ans[0][*row_size]=temp;
    }
    
    
    for(*row_count=1;1;++(*row_count)){
        ans = (double**) realloc(ans,(*row_count+1)*sizeof(double*));
        ans[*row_count] = (double*) malloc((*row_size)*sizeof(double));
            for(m=0;m<*row_size+1;++m){
                scanf("%lf",&temp);
                if(temp!=-1)
                    ans[*row_count][m]=temp;
                if(m==0 && temp==-1){
                    return ans;
                }
            }
    }
}



/*
INPUT:
    double **matrix: data matrix
    int n: number of rows to be printed
    int row_size: horizontal size of the matrix

METHOD:
    This function prints first n row of the matrix.
*/

void print_first_n_row(double **matrix, int n, int row_size) {
    int m,i;
    for(i=0;i<n;++i){
        for(m=0;m<row_size;++m){
            printf("%.4f",matrix[i][m]);
            if(m!=(row_size-1))
                printf(" ");
        }
        if(i!=(n-1))
                printf("\n");
    }
}

/*
INPUT:
    double **matrix: data matrix
    int row_size: horizontal size of the data matrix
    int row1: index of the first row in the calculation
    int row2: index of the second row in the calculation

METHOD:
    This function calculates dot product of the row1 and the row2 and prints it in the following format:
        Dot product of rows <row1>, <row2>: <dot_product>
*/
void calculate_dot_product(double **matrix, int row_size, int row1, int row2) {
    int i;
    double ans;
    for(i=0,ans=0;i<row_size;++i){
        ans += matrix[row1][i]*matrix[row2][i];
    }
    printf("Dot product of rows %d, %d: %.4f",row1,row2,ans);
}

/*
INPUT:
    double **matrix: data matrix
    int row_count: vertical size of the data matrix
    int row_size: horizontal size of the data matrix

OUTPUT:
    double **covariance_matrix: Created covariance matrix with size of row_size X row_size

METHOD:
    This function creates covariance matrix of the original data matrix and returns it.

*/
double **calculate_covariance_matrix(double **matrix, int row_count, int row_size) { 
    int m,n,i;
    double temp;
    double *mean;
    double **ans;
    
    mean=(double*) malloc(row_size*sizeof(double));
    ans=(double**) malloc(row_size*sizeof(double*));
        for(m=0;m<row_size;++m){
            ans[m]=(double*) malloc(row_size*sizeof(double));;
        }
        
    for(m=0;m<row_size;++m){
        for(n=0;n<row_count;++n){
            mean[m]+=matrix[n][m];
        }
        mean[m]=mean[m]/row_count;
    }
    for(m=0;m<row_size;++m){
        for(n=0;n<row_size;++n){
            for(temp=0,i=0;i<row_count;++i){
                temp+=(matrix[i][m]-mean[m])*(matrix[i][n]-mean[n]);
            }
        ans[m][n]=temp/(row_count-1);
        }
    }
    return ans; 
}

/*
INPUT:
    double **matrix: data matrix
    int row_count: vertical size of the data matrix
    int row_size: horizontal size of the data matrix

OUTPUT:
    double **result: Created result matrix with size of row_size X row_size

METHOD:
    This function calculates x^T * x. First finds the transpose of the original data matrix and apply matrix multiplication.
    At the end it returns the calculated matrix with size of row_size X row_size.

*/
double **calculate_x_transpose_times_x(double **matrix, int row_count, int row_size) {
    double **ans;
    int x,y,i;
    ans=(double**) malloc(row_size*sizeof(double*));
        for(i=0;i<row_size;++i){
            ans[i]=(double*) malloc(row_size*sizeof(double));;
        }
    for(x=0;x<row_size;++x){
        for(y=0;y<row_size;++y){
            for(i=0;i<row_count;++i){
                ans[x][y]+=(matrix[i][x]*matrix[i][y]);
            }
        }
    }
    return ans;
}

/*
INPUT:
    double **matrix: data matrix
    int *group_count: number of the groups to be assigned (passed by reference from the main function)
    int row_count: vertical size of the data matrix
    int row_size: horizontal size of the data matrix
    int group_column: index of the column to apply grouping over
    int operation: index of the operation to apply during grouping
        SUM -> 0
        AVG -> 1
        MAX -> 2
        MIN -> 3

OUTPUT:
    double **grouped_matrix: Created grouped matrix with size of group_count X row_size

METHOD:
    This function applies grouping over desired column index, creates a grouped matrix. It returns this grouped matrix.
    During this process it calculates group count and assigns to the variable.

*/
double **group_by(double **matrix, int *group_count, int row_count, int row_size, int group_column, int operation) { 
    double **ans;
    double *unique;
    int x,y,z,new_size,condition,divide;
    
    unique=(double*) malloc(row_count*sizeof(double));
    
    for(x=0;x<row_count;++x){
    unique[x]=-1000000;
    }
    
    for(x=0;x<row_count;++x){
        for(condition=1,y=0;y<row_count;++y){
            if(unique[y]==matrix[x][group_column]){
                condition=0;
            }
        }
        if(condition){
            unique[new_size]=matrix[x][group_column];
            ++new_size;
            }
    }
    
    *group_count=new_size;
    ans=(double**) malloc(new_size*sizeof(double*));
        for(x=0;x<row_count;++x){
            ans[x]=(double*) malloc(row_size*sizeof(double));
        }
    
    for(x=0;x<new_size;++x){
        if(operation==2){for(z=0;z<row_size;++z){ans[x][z]=-1000000;}}/* max init */
        if(operation==3){for(z=0;z<row_size;++z){ans[x][z]= 1000000;}}/* min init */
        for(divide=0,y=0;y<row_count;++y){
            if(unique[x]==matrix[y][group_column] && operation==0)/* sum */{
                for(z=0;z<row_size;++z){
                    ans[x][z]+=matrix[y][z];
                }
            }
            if(unique[x]==matrix[y][group_column] && operation==1)/* average */{
                ++divide;
                for(z=0;z<row_size;++z){
                    ans[x][z]+=matrix[y][z];
                }
            }
            if(unique[x]==matrix[y][group_column] && operation==2)/* max */{
                for(z=0;z<row_size;++z){
                    if(matrix[y][z]>ans[x][z])
                        ans[x][z]=matrix[y][z];
                }
            }
            if(unique[x]==matrix[y][group_column] && operation==3)/* min */{
                for(z=0;z<row_size;++z){
                    if(matrix[y][z]<ans[x][z])
                        ans[x][z]=matrix[y][z];
                }
            }
        }
        if(operation==0)/* sum final */{
            ans[x][group_column]=unique[x];
        }
        if(operation==1)/* avg final */{
            for(z=0;z<row_size;++z){
                    ans[x][z]=ans[x][z]/divide;
                }
            ans[x][group_column]=unique[x];
        }
    }
    return ans;
}

/*
INPUT:
    double **matrix: data matrix
    int row_count: vertical size of the data matrix
    int row_size: horizontal size of the data matrix
    double **kernel: kernel matrix
    int kernel_height: vertical size of the kernel matrix
    int kernel_width: horizontal size of the kernel matrix

OUTPUT:
    double **convoluted_matrix: Created convoluted matrix

METHOD:
    This function applies convolution over data matrix using kernel matrix and returns created matrix.

*/
double **convolution(double **matrix, int row_count, int row_size, double **kernel, int kernel_height, int kernel_width) { 
    int x,y;
    int m,n,p,r;
    double temp;
    double **ans;
    x=row_size-kernel_width+1;
    y=row_count-kernel_height+1;
    
    ans=(double**) malloc(y*sizeof(double*));
        for(m=0;m<row_count;++m){
            ans[m]=(double*) malloc(x*sizeof(double));
        }
    for(m=0;m<y;++m){
        for(n=0;n<x;++n){
            for(temp=0,p=0;p<kernel_height;++p){
                for(r=0;r<kernel_width;++r){
                    temp+=matrix[m+p][n+r]*kernel[p][r];
                }
            }
            ans[m][n]=temp;
        }
    }
    return ans;
}
