#include <mpdecimal.h>

int main (int argc, char** argv) {
	mpd_context_t ctx;
	mpd_t* mpd = mpd_qnew();
	mpd_maxcontext(&ctx);
	mpd_set_string(mpd, argv[1], &ctx);
	/* char* r = mpd_format(mpd, argv[1], &ctx); */
	char* r = mpd_to_sci(mpd, 0);
	char* rEng = mpd_to_eng(mpd, 0);
	printf("%s\n", r);
	printf("%s\n", rEng);
	return 0;
}
