#include <Rcpp.h>
using namespace Rcpp;

/*Este algoritmo de muestre es conocido como de selección - rechazo, y es considerado el mejor algoritmo
para elegir una muestra de acuerdo el muestreo aleatorio simple sin reemplazo, aparte de ser un algoritmo
 completamente secuencial. Aunque tiene la limitación que no siempre llega a elegir por completo los elementos
 necesarios para completar la muestra por lo que en esta versión se incluye la condición de repetir el proceso
 hasta que se complete el vector con el tamaño de la muestra deseado, lo cual limita la velocidad final, pero 
  la aleatoriedad establecida por la elección de valores aleatorios uniformes en (0,1) permite que el algoritmo
  tenga una complejidad lineal O(n).*/

// [[Rcpp::export]]
NumericVector selectReject(NumericVector poblacion,int longMuestra) {
  NumericVector muestra(longMuestra);
  int i,longPoblacion=poblacion.size();
  double aceptados=0.0;
  while (TRUE){
    for (i=0; i<longPoblacion;i++){
      /*Condición de aceptación en el vector muestra que establece que si un cociente particular es mayor 
       que un numero aleatorio generado por una uniforme(0,1) entonces se acepta dentro del vector muestra */
      if ((longMuestra-aceptados)/(longPoblacion-i+1)>runif(1)[0]){  
          aceptados +=1.0;       //en caso de ser aceptado se incrementa en uno esta variable
          muestra[(int)aceptados -1]=poblacion[i];  //Se agrega dicha entra del vector población al vector de la muestra  
      }
    }
    if(aceptados==(double)longMuestra){ //Condición de quiebre en caso llenar el vector muestra
      break;
    }
    else{
      aceptados=0.0;  //En caso de no completarse la muestra reiniciamos el proceso de selección.
    }
  }
  return muestra;
}

/*Este es un ejemplo de su uso en R una vez cargada la función. En donde elegiremos 5 elementos para la 
  establecer nuestra muestra.
 */

/*** R
selectReject(c(1,5,4,6,3,8,5,7,65,789,42,6,36,45,21,56,353,7483,1235,1235,124,87),10)
*/
