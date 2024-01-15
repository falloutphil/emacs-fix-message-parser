#include <quickfix/DataDictionary.h>
#include <quickfix/Message.h>
#include <quickfix/FieldMap.h>
#include <emacs-module.h>
#include <iostream>
#include <string>
#include <stdexcept>

FIX::DataDictionary dictionary("/usr/local/share/quickfix/FIX42.xml");

int plugin_is_GPL_compatible;

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
        int fieldCount = 0;
        emacs_value Qlist = env->intern(env, "nil"); // empty list to store result
        for (FIX::FieldMap::iterator field = fixMessage.begin(); field != fixMessage.end(); ++field) {
            int tag = field->getTag();
            std::string value = field->getString();
            std::string tagName;
            dictionary.getFieldName(tag, tagName);

            emacs_value Qtag = env->make_integer(env, tag);
            emacs_value QtagName = env->make_string(env, tagName.c_str(), tagName.length());
            emacs_value Qvalue = env->make_string(env, value.c_str(), value.length());

            // Create an array for the name-value pair
            emacs_value value_pair_args[] = { QtagName, Qvalue };
            emacs_value QvaluePair = env->funcall(env, env->intern(env, "cons"), 2, value_pair_args);

            // Create an array for the tag-value pair
            emacs_value kv_pair_args[] = { Qtag, QvaluePair };
            emacs_value QkvPair = env->funcall(env, env->intern(env, "cons"), 2, kv_pair_args);

            // Create an array for the cons arguments
            emacs_value cons_args[] = { QkvPair, Qlist };
            Qlist = env->funcall(env, env->intern(env, "cons"), 2, cons_args);

            fieldCount++;
            // Logging each field
            std::cerr << "Field Tag: " << tag << ", Tag Name: " << tagName << ", Value: " << value << std::endl;
        }
        std::cerr << "Total number of fields: " << fieldCount << std::endl;

        // Reverse emacs list to maintain order
        Qlist = env->funcall(env, env->intern(env, "nreverse"), 1, &Qlist);

        return Qlist;
    } catch (const FIX::InvalidMessage& e) {
        emacs_value Qerror_symbol = env->intern(env, "error");
        emacs_value Qerror_message = env->make_string(env, e.what(), strlen(e.what()));
        env->non_local_exit_signal(env, Qerror_symbol, Qerror_message);
        return env->intern(env, "nil"); // empty list
    } catch (const std::exception& e) {
        emacs_value Qerror_symbol = env->intern(env, "error");
        emacs_value Qerror_message = env->make_string(env, e.what(), strlen(e.what()));
        env->non_local_exit_signal(env, Qerror_symbol, Qerror_message);
        return env->intern(env, "nil"); // empty list
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
