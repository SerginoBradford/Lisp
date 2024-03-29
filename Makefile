##
## Some Personnal Project, 2023
## evalExpr
## File description:
## Makefile
##

#COLOR
RED				=	\e[31m
GREEN			=	\e[32m
YELLOW			=	\e[33m
DEFAULT			=	\e[0m
BOLD			=	\e[1m

#TEXT
COMPILE			=	$$'$(BOLD)$(GREEN)[ Compiled the project ! ]\t$(DEFAULT)'
RE_COMPILE		=	$$'$(BOLD)$(GREEN)[ Recompiled the project ! ]\t$(DEFAULT)'
CLEAN			=	$$'$(BOLD)$(RED)[ Removed useless files... ]\t$(DEFAULT)'
FCLEAN			=	$$'$(BOLD)$(RED)[ Removed binary... ]\t$(DEFAULT)'
TESTS_COMPILE	=	$$'$(BOLD)$(YELLOW)[ Excecuted Units tests... ]\t$(DEFAULT)'

SRCS	=	$(shell find ./app ./src -name "*.hs")

TESTS	=	$(shell find ./test -name '*.hs')

BENCHS	=	$(shell find ./benchmark -name '*.hs')

USELESS		=	$(shell find ./test -type f -name "*~" -o -name "*.o" -o -name "*.hi" -o -name "*.dyn_hi" -o -name "*.dyn_o" -o -name "\#*\#")

BIN_DIR		=	$(shell stack path --local-install-root)

UNIT_TESTS	=	unit_tests

NAME		=	lisp

all:	$(SRCS)
	@stack build
	@cp $(BIN_DIR)/bin/$(NAME)-exe $(NAME)
	@echo $(COMPILE)

tests_run:	$(TESTS)
	@stack test

bench_run:	$(BENCHS)
	@stack bench

clean:
	@stack clean --full
	@rm -rf $(USELESS)
	@echo $(CLEAN)

fclean: clean
	@rm -f $(UNIT_TESTS) $(NAME)
	@echo $(FCLEAN)

re: fclean all
	@echo $(RE_COMPILE)

.PHONY: all tests_run clean fclean re
