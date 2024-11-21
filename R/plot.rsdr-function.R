plot.rsdr=
  function(
    data
    ,w_cutoff=0
    ,label=F
    ,label_digits=2
    ,label_size=3
    ,font_family='sans'
    ,font_size=9
    ,input_dim_order=NULL
    ,output_dim_order=NULL
    ,input_dim_desc=NULL
    ,output_dim_desc=NULL
  ){
    # data = composition(x)
    if(!is.null(input_dim_order)){
      data=
        data %>%
        .[input_dim_order[input_dim_order%in%rownames(.)],,drop=F]
    }
    
    if(!is.null(output_dim_order)){
      data=
        data %>%
        .[,output_dim_order[output_dim_order%in%colnames(.)],drop=F]
    }
    
    data=
      data %>%
      rownames_to_column(var='input_dim') %>%
      gather(output_dim,weight,-input_dim)
    
    if(!is.null(input_dim_desc)){
      data=
        data %>%
        left_join(input_dim_desc,by='input_dim') %>%
        unite(input_dim,input_dim_desc,input_dim,sep=' - ')
    }
    
    if(!is.null(output_dim_desc)){
      data=
        data %>%
        left_join(output_dim_desc,by='output_dim') %>%
        unite(output_dim,output_dim_desc,output_dim,sep=' - ')
    }
    
    data=
      data %>%
      mutate(
        input_dim=
          input_dim %>%
          factor(unique(.))
        ,output_dim=
          output_dim %>%
          factor(unique(.))
      ) %>%
      mutate(
        weight=
          ifelse(
            abs(weight)<=w_cutoff
            ,0,weight
          )
      )
    
    p=data %>%
      qplot(output_dim,input_dim,fill=weight,data=.,geom='tile')
    
    if(label){
      p=p +
        geom_text(
          aes(
            label=ifelse(weight==0,NA,round(weight,label_digits))
            ,alpha=abs(weight)
          )
          ,size=label_size
          ,color='#FFFFFF'
          ,family=font_family
          ,na.rm=T
        )
    }
    
    p +
      scale_x_discrete('Output dimension') +
      scale_y_discrete('Input dimension') +
      scale_fill_gradient2(
        paste0('Cutoff\n<=|',w_cutoff,'|')
        ,low='red'
        ,mid='black'
        ,high='green'
        ,midpoint=0
      ) +
      scale_alpha(guide=F) +
      theme(
        axis.title=element_text(family=font_family,size=font_size)
        ,axis.text=element_text(family=font_family,size=font_size)
        ,axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)
        ,axis.text.y=element_text(angle=0,hjust=1,vjust=0.5)
        ,legend.title.align=0.5
        ,legend.title=element_text(family=font_family,size=font_size)
        ,legend.text=element_text(family=font_family,size=font_size)
      )
  }