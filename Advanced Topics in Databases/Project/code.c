#include <Yap/YapInterface.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <libpq-fe.h>
#include <math.h>

void unravel_polygon(char *out, YAP_Term yapList){
	YAP_Term head, tail, head_points, tail_points;

	if (!Yap_IsListTerm(yapList))
	{
		return false;
	}

	head = YAP_HeadOfTerm(yapList);
	tail_points = YAP_TailOfTerm(yapList);

	strcpy(out, "'POLYGON(");

	//printf("%s\n", out);

	int n_holes = YAP_IntOfTerm(YAP_HeadOfTerm(head));
	//printf("%d hole(s)\n", n_holes);
	tail = YAP_TailOfTerm(head);

	//printf("Tail unbroken\n");

	int n_points[n_holes];

	//printf("Entering cycle\n");

	for (int i = 0; i < n_holes; i++)
	{
		//printf("getting points\n");
		if (YAP_IsPairTerm(tail))
		{
			head = YAP_HeadOfTerm(tail);
			tail = YAP_TailOfTerm(tail);
		}
		else {
			head = tail;
		}
		n_points[i] = YAP_IntOfTerm(head);
		//printf("got points %d\n", n_points[i]);
	}

	for (int i = 0; i < n_holes; i++)
	{
		strcat(out, "(");
		for (int j = 0; j < n_points[i]; j++)
		{


			head_points = YAP_HeadOfTerm(tail_points);
			tail_points = YAP_TailOfTerm(tail_points);

			strcat(out, (char *) YAP_AtomName(YAP_AtomOfTerm(head_points)));
			if (n_points[i] - j > 1)
			{
				strcat(out, ",");
			}

		}
		strcat(out, ")");
		if (n_holes-i>1)
		{
			strcat(out, ",");
		}
	}

	strcat(out, ")'");
}

YAP_Term print_piece(PGresult *res, int row){

	int n_points = atoi(PQgetvalue(res, row, 0));
	int total_points = n_points;

	YAP_Term points = YAP_MkPairTerm(YAP_MkIntTerm(1), YAP_MkIntTerm(total_points));

	char *polygon = PQgetvalue(res, row, 1);
	polygon += 9;

	YAP_Term list[total_points];

	char *point = strtok(polygon, ",");

	while(point != NULL && n_points > 0)
	{
		if (n_points == 1)
		{
			point[strlen(point)-2] = '\0';
		}

		list[total_points-n_points] = YAP_MkAtomTerm(YAP_LookupAtom(point));
		point = strtok(NULL, ",");
		n_points-=1;
	}

	return (YAP_MkPairTerm(points, YAP_MkListFromTerms(list, total_points)));
}

YAP_Term print_puzzle(PGresult *res, int row){

	int n_points = atoi(PQgetvalue(res, row, 1));
	int total_points = n_points;
	int components = atoi(PQgetvalue(res, row, 3));

	if (components == 0)
	{
		YAP_Term points = YAP_MkPairTerm(YAP_MkIntTerm(1), YAP_MkIntTerm(total_points));

		char *polygon = PQgetvalue(res, row, 2);
		polygon += 9;

		YAP_Term list[total_points];

		char *point = strtok(polygon, ",");

		while(point != NULL && n_points > 0)
		{
			if (n_points == 1)
			{
				point[strlen(point)-2] = '\0';
			}

			list[total_points-n_points] = YAP_MkAtomTerm(YAP_LookupAtom(point));
			point = strtok(NULL, ",");
			n_points-=1;
		}

		return (YAP_MkPairTerm(points, YAP_MkListFromTerms(list, total_points)));

	} else if(components > 0) {
		YAP_Term point_counts[components+2];
		point_counts[0] = YAP_MkIntTerm(components+1);

		YAP_Term list[total_points];
		int comp = components+1;
		int i = 0;

		char *polygon = PQgetvalue(res, row, 2);
		polygon += 9;

		char *point = strtok(polygon, ",(");

		while(point != NULL && n_points > 0)
		{

			if (n_points == 1)
			{
				//printf("end of component and polygon %d with %d points\n", components-comp+1, i);
				point[strlen(point)-1] = '\0';
				point_counts[components-comp+2] = YAP_MkIntTerm(i);
			}

			if(point[strlen(point)-1] == ')' && n_points>1)
			{
				//printf("end of component %d with %d points\n", components-comp+1, i);
				point[strlen(point)-1] = '\0';
				point_counts[components-comp+2] = YAP_MkIntTerm(i);
				comp-=1;
				i=0;
			}

			i++;
			//printf("%s\n", point);
			list[total_points-n_points] = YAP_MkAtomTerm(YAP_LookupAtom(point));
			point = strtok(NULL, ",(");
			n_points-=1;
			//printf("%d\n", n_points);
		}

		YAP_Term points = YAP_MkListFromTerms(point_counts, components+2);

		//printf("Error not in points\n");

		return (YAP_MkPairTerm(points, YAP_MkListFromTerms(list, total_points)));
	}

	return false;
}

static int db_connect(void){

	YAP_Term user = YAP_ARG1;
	YAP_Term pwd = YAP_ARG2;
	YAP_Term db = YAP_ARG3;
	YAP_Term out = YAP_ARG4;

	if(!YAP_IsAtomTerm(user)){
		printf("Erro de: user");
		return false;
	}

	if(!YAP_IsAtomTerm(pwd)){
		printf("Erro de: pwd");
		return false;
	}

	if(!YAP_IsAtomTerm(db)){
		printf("Erro de: db");
		return false;
	}

	if(!YAP_IsVarTerm(out)){
		printf("Erro de: out");
		return false;
	}

	char *user_ts;
	char *pwd_ts;
	char *db_ts;

	user_ts = (char *) YAP_AtomName(YAP_AtomOfTerm(user));
	//printf("%s\n", user_ts);

	pwd_ts = (char *) YAP_AtomName(YAP_AtomOfTerm(pwd));
	//printf("%s\n", pwd_ts);

	db_ts = (char *) YAP_AtomName(YAP_AtomOfTerm(db));
	//printf("%s\n", db_ts);

	PGconn *conn = PQsetdbLogin("localhost", NULL, NULL, NULL, db_ts, user_ts, pwd_ts);


	if (PQstatus(conn) != CONNECTION_OK){
		printf("Connection failed\n");
		fprintf(stderr, "Connection to database failed: %s\n", PQerrorMessage(conn));
        PQfinish(conn);
        return false;
	}

	YAP_Term chandler = YAP_MkIntTerm((long) conn);

    return(YAP_Unify(out,chandler));
}

static int db_disconnect(void){
	YAP_Term con_arg = YAP_ARG1;
	PGconn *conn = (PGconn *) YAP_IntOfTerm(con_arg);

	PQfinish(conn);
	return true;
}

static int db_query(void){
	YAP_Term con_arg = YAP_ARG1;
	YAP_Term qry_arg = YAP_ARG2;
	YAP_Term out = YAP_ARG3;


	PGconn *conn = (PGconn *) YAP_IntOfTerm(con_arg);

	if (PQstatus(conn) != CONNECTION_OK)
	{
		printf("connection not ok\n");
		PQreset(conn);
	}


	char *qry = (char *) YAP_AtomName(YAP_AtomOfTerm(qry_arg));
	//printf("%s\n", qry);

	PGresult *result = PQexec(conn, qry);

	if (PQresultStatus(result) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(result));
    	fprintf(stderr, "%s\n", PQerrorMessage(conn));
	    PQclear(result);
	    return false;
    }

	YAP_Term res = YAP_MkIntTerm((long) result);


	return(YAP_Unify(out, res));
}

static int get_piece(void){
	YAP_Term res_arg = YAP_ARG1;
	YAP_Term row_arg = YAP_ARG2;
	YAP_Term out = YAP_ARG3;

	PGresult *res = (PGresult *) YAP_IntOfTerm(res_arg);
	int row_n = YAP_IntOfTerm(row_arg);

	if (row_n > PQntuples(res))
	{
		return false;
	}

	//printf("in row number\n");

	row_n -= 1;
	YAP_Term polygon = print_piece(res, row_n);

	return(YAP_Unify(out,polygon));
}

static int db_get_pieces(void){
	YAP_Term con_arg = YAP_ARG1;
	YAP_Term out = YAP_ARG2;

	PGconn *conn = (PGconn *) YAP_IntOfTerm(con_arg);

	if (PQstatus(conn) != CONNECTION_OK)
	{
		printf("connection not ok\n");
		PQreset(conn);
	}

	char *qry = "SELECT ST_NPoints(shape), ST_AsText(shape) FROM pieces;";

	PGresult *result = PQexec(conn, qry);

	if (PQresultStatus(result) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(result));
    	fprintf(stderr, "%s\n", PQerrorMessage(conn));
	    PQclear(result);
	    return false;
    }

	YAP_Term res = YAP_MkIntTerm((long) result);

	return(YAP_Unify(out, res));
}

static int db_free_res(void){
	YAP_Term res_ts = YAP_ARG1;

	PGresult *result = (PGresult *) YAP_IntOfTerm(res_ts);

	PQclear(result);

	return true;
}

static int db_res_count(void){
	YAP_Term res_ts = YAP_ARG1;
	YAP_Term out = YAP_ARG2;

	PGresult *res = (PGresult *) YAP_IntOfTerm(res_ts);

	int res_c = PQntuples(res);
	YAP_Term res_yap = YAP_MkIntTerm(res_c);

	return(YAP_Unify(out, res_yap));
}

static int db_get_solutions(void){
	YAP_Term con_arg = YAP_ARG1;
	YAP_Term puzzle_arg = YAP_ARG2;
	YAP_Term out = YAP_ARG3;

	PGconn *conn = (PGconn *) YAP_IntOfTerm(con_arg);

	if (PQstatus(conn) != CONNECTION_OK)
	{
		printf("connection not ok\n");
		PQreset(conn);
	}

	char qry[70] = "SELECT id_piece, ST_X(pT), ST_Y(pT), pR FROM solver where id_puzzle = ";
	char num[4];
	sprintf(num, "%d", YAP_IntOfTerm(puzzle_arg));
	strcat(qry, num);
	strcat(qry, ";");

	PGresult *result = PQexec(conn, qry);

	if (PQresultStatus(result) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(result));
    	fprintf(stderr, "%s\n", PQerrorMessage(conn));
	    PQclear(result);
	    return false;
    }

	YAP_Term res = YAP_MkIntTerm((long) result);

	return(YAP_Unify(out, res));
}

static int db_get_puzzles(void){
	YAP_Term con_arg = YAP_ARG1;
	YAP_Term out = YAP_ARG2;

	PGconn *conn = (PGconn *) YAP_IntOfTerm(con_arg);

	if (PQstatus(conn) != CONNECTION_OK)
	{
		printf("connection not ok\n");
		PQreset(conn);
	}

	char *qry = "SELECT id, ST_NPoints(shape), ST_AsText(shape), ST_NumInteriorRings(shape) FROM puzzles;";

	PGresult *result = PQexec(conn, qry);

	if (PQresultStatus(result) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(result));
    	fprintf(stderr, "%s\n", PQerrorMessage(conn));
	    PQclear(result);
	    return false;
    }

	YAP_Term res = YAP_MkIntTerm((long) result);

	return(YAP_Unify(out, res));
}

static int print_polygon(void){
	YAP_Term to_print = YAP_ARG1;
	char result[1000];

	unravel_polygon(result, to_print);

	printf("%s\n\n\n", result);

	return true;
}

static int get_puzzle(void){
	YAP_Term res_arg = YAP_ARG1;
	YAP_Term puzzle_arg = YAP_ARG2;
	YAP_Term out = YAP_ARG3;

	PGresult *res = (PGresult *) YAP_IntOfTerm(res_arg);

	if (PQresultStatus(res) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(res));
	    PQclear(res);
	    return false;
    }

	int puzzle_n = YAP_IntOfTerm(puzzle_arg);

	if (puzzle_n > PQntuples(res))
	{
		printf("Puzzle not in result\n");
		return false;
	}

	puzzle_n -= 1;
	YAP_Term polygon = print_puzzle(res, puzzle_n);

	return(YAP_Unify(out,polygon));

}

static int get_solution(void){
	YAP_Term res_arg = YAP_ARG1;
	YAP_Term piece_arg = YAP_ARG2;
	YAP_Term out = YAP_ARG3;

	PGresult *res = (PGresult *) YAP_IntOfTerm(res_arg);

	if (PQresultStatus(res) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(res));
	    PQclear(res);
	    return false;
    }

    int piece_id = YAP_IntOfTerm(piece_arg);

    if (piece_id > PQntuples(res))
	{
		printf("Piece solution not in result\n");
		return false;
	}

	YAP_Term list[3];

	for (int i = 0; i < 3; ++i)
	{
		list[i] = YAP_MkFloatTerm(atof(PQgetvalue(res, piece_id, i+1)));
	}

	return(YAP_Unify(out, YAP_MkListFromTerms(list, 3)));
}

static int st_difference(void){
	YAP_Term con_arg = YAP_ARG1;
	YAP_Term poly_arg1 = YAP_ARG2;
	YAP_Term poly_arg2 = YAP_ARG3;
	YAP_Term out = YAP_ARG4;

	PGconn *conn = (PGconn *) YAP_IntOfTerm(con_arg);

	if (PQstatus(conn) != CONNECTION_OK)
	{
		printf("connection not ok\n");
		PQreset(conn);
	}

	char poly_1[1000];
	unravel_polygon(poly_1, poly_arg1);

	char qry[2300] = "WITH cte as(SELECT st_difference(st_geomfromtext(";
	strcat(qry, poly_1);
	strcat(qry, "), st_geomfromtext(");
	memset(poly_1, '\0', sizeof(poly_1));
	unravel_polygon(poly_1, poly_arg2);
	strcat(qry, poly_1);
	strcat(qry, ")) as new_piece) SELECT new_piece, ST_NPoints(new_piece), ST_AsText(new_piece), ST_NumInteriorRings(new_piece) FROM cte;");

	PGresult *result = PQexec(conn, qry);

	if (PQresultStatus(result) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(result));
    	fprintf(stderr, "%s\n", PQerrorMessage(conn));
	    PQclear(result);
	    return false;
    }

    YAP_Term polygon = print_puzzle(result, 0);

    return(YAP_Unify(out, polygon));
}

static int st_covers(void){
	YAP_Term con_arg = YAP_ARG1;
	YAP_Term poly_arg1 = YAP_ARG2;
	YAP_Term poly_arg2 = YAP_ARG3;

	PGconn *conn = (PGconn *) YAP_IntOfTerm(con_arg);

	if (PQstatus(conn) != CONNECTION_OK)
	{
		printf("connection not ok\n");
		PQreset(conn);
		return false;
	}

	char poly_1[1000], poly_2[1000];
	unravel_polygon(poly_1, poly_arg1);
	unravel_polygon(poly_2, poly_arg2);

	char qry[2300] = "Select st_covers(st_geomfromtext(";
	strcat(qry, poly_1);
	strcat(qry, "), st_geomfromtext(");
	strcat(qry, poly_2);
	strcat(qry, "));");

	//printf("%s\n", qry);

	PGresult *result = PQexec(conn, qry);

	if (PQresultStatus(result) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(result));
    	fprintf(stderr, "%s\n", PQerrorMessage(conn));
	    PQclear(result);
	    return false;
    }

    //printf("so far so good \n");

    char *res = PQgetvalue(result, 0, 0);

    PQclear(result);

    //printf("%s\n", res);

    if (strstr(res, "t") != NULL)
    {
    	return true;
    }
    else return false;
}

static int st_translate(void){
	YAP_Term con_arg = YAP_ARG1;
	YAP_Term poly_arg = YAP_ARG2;
	YAP_Term sol_arg = YAP_ARG3;
	YAP_Term out = YAP_ARG4;

	PGconn *conn = (PGconn *) YAP_IntOfTerm(con_arg);

	if (PQstatus(conn) != CONNECTION_OK)
	{
		printf("connection not ok\n");
		PQreset(conn);
		return false;
	}

	char poly_1[1000];
	unravel_polygon(poly_1, poly_arg);

	char qry[1500] = "WITH cte as (select st_translate(st_geomfromtext(";
	strcat(qry, poly_1);
	strcat(qry, "), ");

	YAP_Term head, tail;
	head = YAP_HeadOfTerm(sol_arg);
	tail = YAP_TailOfTerm(sol_arg);

	float c = YAP_FloatOfTerm(head);

	gcvt(c, 16, qry+strlen(qry));

	head = YAP_HeadOfTerm(tail);
	tail = YAP_TailOfTerm(tail);

	c = YAP_FloatOfTerm(head);

	strcat(qry, ", ");

	gcvt(c, 16, qry+strlen(qry));

	strcat(qry, ") as new_piece) SELECT ST_NPoints(new_piece), ST_AsText(new_piece) FROM cte;");

	PGresult *result = PQexec(conn, qry);

	if (PQresultStatus(result) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(result));
    	fprintf(stderr, "%s\n", PQerrorMessage(conn));
	    PQclear(result);
	    return false;
    }

	YAP_Term transpoly = print_piece(result, 0);

	return(YAP_Unify(out, transpoly));
}

static int st_rotate(void){
	YAP_Term con_arg = YAP_ARG1;
	YAP_Term poly_arg = YAP_ARG2;
	YAP_Term sol_arg = YAP_ARG3;
	YAP_Term out = YAP_ARG4;

	PGconn *conn = (PGconn *) YAP_IntOfTerm(con_arg);

	if (PQstatus(conn) != CONNECTION_OK)
	{
		printf("connection not ok\n");
		PQreset(conn);
		return false;
	}

	char poly_1[1000];
	unravel_polygon(poly_1, poly_arg);

	double c;
	char qry[1500] = "WITH cte as (select st_rotate(st_geomfromtext(";
	strcat(qry, poly_1);
	strcat(qry, "), ");

	YAP_Term head, tail;
	tail = YAP_TailOfTerm(sol_arg);
	head = YAP_HeadOfTerm(sol_arg);

	//printf("please %f\n", YAP_FloatOfTerm(head));

	tail = YAP_TailOfTerm(tail);
	head = YAP_HeadOfTerm(tail);

	c = YAP_FloatOfTerm(head);

	//printf("something's wrong, I can feel %f\n", c);

	c = c * 3.14159265359;

	//printf("Multiplied by PI %f\n", c);

	gcvt(c, 16, qry+strlen(qry));

	strcat(qry, ", ");

	tail = YAP_TailOfTerm(poly_arg);
	head = YAP_HeadOfTerm(tail);

	memset(poly_1, '\0', sizeof(poly_1));

	strcpy(poly_1, YAP_AtomName(YAP_AtomOfTerm(head)));

	strcat(qry, "st_geomfromtext('POINT(");
	strcat(qry, poly_1);
	strcat(qry, ")')) as new_piece) SELECT ST_NPoints(new_piece), ST_AsText(new_piece) FROM cte;");

	//printf("Sending query\n");

	PGresult *result = PQexec(conn, qry);

	if (PQresultStatus(result) != PGRES_TUPLES_OK)
    {
    	printf("Failed to get result\n");
    	printf("%s\n", PQresultErrorMessage(result));
    	fprintf(stderr, "%s\n", PQerrorMessage(conn));
	    PQclear(result);
	    return false;
    }

    //printf("result obtained\n");

	YAP_Term transpoly = print_piece(result, 0);

	return(YAP_Unify(out, transpoly));
}

void init_dbs(void){
	YAP_UserCPredicate("db_disconnect",db_disconnect,1);
	YAP_UserCPredicate("db_connect",db_connect,4);
	YAP_UserCPredicate("db_query",db_query,3);
	YAP_UserCPredicate("db_get_pieces", db_get_pieces, 2);
	YAP_UserCPredicate("get_piece", get_piece, 3);
	YAP_UserCPredicate("db_free_res", db_free_res, 1);
	YAP_UserCPredicate("db_res_count", db_res_count, 2);
	YAP_UserCPredicate("db_get_solutions", db_get_solutions, 3);
	YAP_UserCPredicate("db_get_puzzles", db_get_puzzles, 2);
	YAP_UserCPredicate("print_polygon", print_polygon, 1);
	YAP_UserCPredicate("get_puzzle", get_puzzle, 3);
	YAP_UserCPredicate("get_solution", get_solution, 3);
	YAP_UserCPredicate("st_difference", st_difference, 4);
	YAP_UserCPredicate("st_covers", st_covers, 3);
	YAP_UserCPredicate("st_translate", st_translate, 4);
	YAP_UserCPredicate("st_rotate", st_rotate, 4);

}

/*
################################
Documentação de comandos:
################################


db_disconnect recebe 1 argumento de input: connection handler (que foi typecast para um int, como todos os connection handlers mencionados no futuro):
	fecha o connection handler;

db_connect recebe 3 argumentos de input e 1 de output: username, password e base de dados a aceder | output:
	no output 4 guarda o connection handler no output se a conexão suceder;

db_query recebe 2 argumentos de input e 1 de output: connection handler e um atom (query) | output:
	se a query retornar valores, guarda o result set (typecast para int, como todos os result sets mencionados no futuro) no argumento de output;

db_get_pieces recebe 1 argumento de input e 1 de output: connection handler | output:
	se o connection handler estiver correto, executa uma query pre-formatada para obter todas as peças e guarda o result set no output;

get_piece recebe 2 argumentos de input e 1 de output: result set e numero da peça (index começa em 1) | output:
	se o numero da peça estiver no result set, transforma a peça num output da forma usada para guardar poligonos no YAP
				 	[[numero de buracos na peça (0) | numero de pontos da peça | numero de pontos de cada buraco (n/a neste caso)] | lista de pontos]

db_free_res recebe 1 argumento de input: result set:
	liberta o result set;

db_res_count recebe 1 argumento de input e 1 de output: result set | output:
	guarda o numero de linhas (rows) num dado result set no argumento de output;

db_get_solutions recebe 2 argumentos de input e 1 de output: connection handler e numero do puzzle | output:
	se o connection handler estiver correto e se o puzzle existir, guarda o result set com as soluções do puzzle no output.

db_get_puzzles recebe 1 argumento de input e 1 de output: connection handler | output:
	se o connection handler estiver correto, guarda o result set com todos os puzzles de um modo interpretável por get_puzzle;

print_polygon recebe 1 argumento de input: um termo formatado corretamente como a forma de guardar poligonos:
	se o poligono estiver bem formatado, escreve a string que seria usada para representar o poligono dentro de uma função ST_GeomFromText();

get_puzzle recebe 2 argumentos de input e 1 de output: o result set de db_get_puzzles, o número do puzzle | output:
	se o numero do puzzle estiver no result set, guarda o poligono do puzzle na forma descrita em get_piece no output

get_solution recebe 2 argumentos de input e 1 de output: o result set de db_get_solutions, o número da peça | output:
	se o numero da peça estiver no result set, guarda a translação e a rotação da peça no output da forma
					[translação_X, translação_Y, rotação em PI radianos (multiplicada por PI em st_rotate)]

st_difference recebe 3 argumentos de input e 1 de output: connection handler, poligono 1, poligono 2 | output:
	se o connection handler estiver correto e os poligonos também, guarda o resultado de fazer st_difference(poligono 1, poligono 2) no output

st_covers recebe 3 argumentos de input: connection handler, poligono 1, poligono 2:
	se o connection handler estiver correto, retorna o resultado de st_covers(poligono 1, poligono 2)

st_translate recebe 3 argumentos de input e 1 de output: connection handler, poligono, conjunto de solução | output:
	se o connection handler estiver correto, guarda o resultado de st_translate(poligono, CS[0], CS[1]) no output

st_rotate recebe 3 argumentos de input e 1 de output: connection handler, poligono, conjunto de solução | output:
	se o connection handler estiver correto, guarda o resultado de st_rotate(poligono, CS[2]*PI, primeiro ponto do poligono) no output

*/
