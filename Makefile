CC= 	fsc

SRC= 	computorV1.scala

OBJ= 	./classes

NAME= 	computor

all: 	$(NAME)

$(NAME):
	echo '#!/bin/zsh' > $@
	echo 'scala -classpath $(OBJ) $(NAME) $$argv' >> $@
	chmod +x $(NAME)
	mkdir $(OBJ)
	$(CC) -d $(OBJ) $(SRC)

clean:

fclean:
	rm $(NAME)
	rm -rf $(OBJ)

re: fclean all
