NAME = computor

SRCS = src/computor_v1/core.clj \
	   src/computor_v1/polynom.clj

JAR = target/uberjar/computor-v1-0.1.0-SNAPSHOT-standalone.jar

all: $(NAME)

$(NAME): $(JAR)
	@echo '[[ -f target/uberjar/computor-v1-0.1.0-SNAPSHOT-standalone.jar ]] && java -jar target/uberjar/computor-v1-0.1.0-SNAPSHOT-standalone.jar "$$1"' > computor
	@chmod +x $(NAME)


$(JAR): $(SRCS)
	lein uberjar

clean:
	rm $(JAR)

fclean: clean
	rm $(NAME)

re: fclean all
