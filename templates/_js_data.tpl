{% javascript %}
mod_yaml_import.constants = {
    urls: {
        URL_GETDATA: "/api/yaml_import/get_data",
        URL_VALIDATE: "/api/yaml_import/validate_setting"
    },
    types: [
        {
            title: "{_ Text _}",
            id: "text",
            info: "This is the default."
        },
/*
        {
            title: "Textarea",
            id: "textarea",
            info: "Multiline text; useful when generating the admin template."
        },
        {
            title: "Date",
            id: "date",
            info: "Not implemented yet."
        },
*/
        {
            title: "Status",
            id: "status",
            info: "Sets the page status.",
            mapping: [
                {
                    title: "is_published",
                    id: "is_published"
                },
                {
                    title: "is_featured",
                    id: "is_featured"
                },
                {
                    title: "is_protected",
                    id: "is_protected"
                }
            ]
        },
        {
            title: "Medium",
            id: "medium",
            info: "Media will be downloaded and connected to the page. Value is a URL or an array of URLs."
        },
        {
            title: "Page connection",
            id: "connection",
            info: "Connect to other pages using a predicate.",
            mapping: []
        }
    ]
};
{% endjavascript %}