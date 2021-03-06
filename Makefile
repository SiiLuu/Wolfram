##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## Makefile
##

SRC  =  src/main.hs			\
		src/tree.hs			\
		src/arguments.hs	\
		src/default.hs		\
		src/rule30.hs		\
		src/rule90.hs 		\
		src/rule110.hs

OBJ   =  $(SRC:.cpp=.o)

NAME =  wolfram

all:  $(NAME)

$(NAME):  $(OBJ)
	ghc -o $(NAME) $(OBJ)

%.o : %.cpp
	ghc -o $@ -c $<

clean:
	rm -f src/*.o
	rm -f src/*.hi

fclean:  clean
	rm -f $(NAME)

re:   fclean all

debug:  CFLAGS += -g3
debug:  fclean all