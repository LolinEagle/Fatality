all:
	@dune build @install
	@dune install --prefix .
	@echo "Building done"

clean:
	@dune clean
	@rm -rf _build lib doc automaton
	@echo "Cleaning done"

fclean:clean
	@rm -rf bin

re:fclean
	@make all

run:re
	./bin/ft_ality grammars/valid/mk9.gmr

test:re
	@for file in grammars/error/*.gmr; do \
		echo "== $$(basename $$file) =="; \
		./bin/ft_ality $$file || true; \
	done

.PHONY:all clean fclean re run test
