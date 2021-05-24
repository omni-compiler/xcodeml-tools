#if defined(__cplusplus)
extern "C"
{
#endif

int execute_args(int argc, char *argv[]);

#if defined(__cplusplus)
}
#endif

int main(int argc, char *argv[])
{
    execute_args(argc, argv);
}
