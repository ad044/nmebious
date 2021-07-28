FROM nginx

COPY ./nginx/nginx.conf.template /etc/nginx/conf.d/default.conf.template

COPY ./scripts/run-nginx.sh /

ENTRYPOINT ["/run-nginx.sh"]

CMD ["nginx", "-g", "daemon off;"]