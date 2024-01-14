#include <quickfix/Message.h>
#include <quickfix/FieldMap.h>
#include <emacs-module.h>
#include <vector>
#include <iostream>
#include <string>
#include <stdexcept>

int plugin_is_GPL_compatible;

struct TagValue {
    int tag;
    std::string value;
};

static emacs_value Fparse_fix_message(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
    try {
        // Extract the FIX message string from Emacs argument
        ptrdiff_t size = 0;
        env->copy_string_contents(env, args[0], NULL, &size);
        char *message_str = new char[size];
        env->copy_string_contents(env, args[0], message_str, &size);

        std::string fixMessageStr(message_str);
        delete[] message_str;

        FIX::Message fixMessage;
        fixMessage.setString(fixMessageStr);

        // Logging the number of fields
        std::vector<TagValue> tags;
        int fieldCount = 0;
        for (FIX::FieldMap::iterator field = fixMessage.begin(); field != fixMessage.end(); ++field) {
            tags.push_back(TagValue{field->getTag(), field->getString()});
            fieldCount++;
            // Logging each field
            std::cerr << "Field Tag: " << field->getTag() << ", Value: " << field->getString() << std::endl;
        }

        std::cerr << "Total number of fields: " << fieldCount << std::endl;

        emacs_value Qlist = env->intern(env, "nil");
        for (const auto& tag : tags) {
            emacs_value Qtag = env->make_integer(env, tag.tag);
            emacs_value Qvalue = env->make_string(env, tag.value.c_str(), tag.value.length());

            // Create an array for the tag-value pair
            emacs_value pair_args[] = { Qtag, Qvalue };
            emacs_value Qpair = env->funcall(env, env->intern(env, "cons"), 2, pair_args);

            // Create an array for the cons arguments
            emacs_value cons_args[] = { Qpair, Qlist };
            Qlist = env->funcall(env, env->intern(env, "cons"), 2, cons_args);
            // Logging the Emacs alist construction
            std::cerr << "Emacs Alist - Tag: " << tag.tag << ", Value: " << tag.value << std::endl;
        }

        // Reverse emacs list to maintain order
        Qlist = env->funcall(env, env->intern(env, "nreverse"), 1, &Qlist);

        return Qlist;
    } catch (const std::exception& e) {
        emacs_value Qerror_symbol = env->intern(env, "error");
        emacs_value Qerror_message = env->make_string(env, e.what(), strlen(e.what()));
        env->non_local_exit_signal(env, Qerror_symbol, Qerror_message);
        return env->intern(env, "nil");
    }
}

extern "C" int emacs_module_init(struct emacs_runtime *ert) {
    emacs_env *env = ert->get_environment(ert);
    emacs_value f = env->make_function(env, 1, 1, Fparse_fix_message, "Parse FIX message", NULL);
    emacs_value Qfset = env->intern(env, "fset");
    emacs_value Qparse_fix_message = env->intern(env, "parse-fix-message");
    emacs_value args[] = { Qparse_fix_message, f };
    env->funcall(env, Qfset, 2, args);
    return 0;
}
