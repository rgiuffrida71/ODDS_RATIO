
library(shiny)
library(ggplot2)

ui <- fluidPage(
	titlePanel("Odds Ratio (IC 95%) e Forest Plot"),
	
	sidebarLayout(
		sidebarPanel(
			width = 4,
			
			# Fator de Risco 1
			wellPanel(
				textInput("nome1", "Fator de Risco 1", value = "Fator de Risco 1"),
				fluidRow(
					column(6, numericInput("a1", "Exposto / Desfecho (+)", value = 45, min = 0)),
					column(6, numericInput("b1", "Exposto / Desfecho (-)", value = 55, min = 0))
				),
				fluidRow(
					column(6, numericInput("c1", "Não Exp. / Desfecho (+)", value = 20, min = 0)),
					column(6, numericInput("d1", "Não Exp. / Desfecho (-)", value = 80, min = 0))
				)
			),
			
			# Fator de Risco 2
			wellPanel(
				textInput("nome2", "Fator de Risco 2", value = "Fator de Risco 2"),
				fluidRow(
					column(6, numericInput("a2", "Exposto / Desfecho (+)", value = 30, min = 0)),
					column(6, numericInput("b2", "Exposto / Desfecho (-)", value = 70, min = 0))
				),
				fluidRow(
					column(6, numericInput("c2", "Não Exp. / Desfecho (+)", value = 25, min = 0)),
					column(6, numericInput("d2", "Não Exp. / Desfecho (-)", value = 75, min = 0))
				)
			),
			
			# Fator de Risco 3
			wellPanel(
				textInput("nome3", "Fator de Risco 3", value = "Fator de Risco 3"),
				fluidRow(
					column(6, numericInput("a3", "Exposto / Desfecho (+)", value = 15, min = 0)),
					column(6, numericInput("b3", "Exposto / Desfecho (-)", value = 85, min = 0))
				),
				fluidRow(
					column(6, numericInput("c3", "Não Exp. / Desfecho (+)", value = 35, min = 0)),
					column(6, numericInput("d3", "Não Exp. / Desfecho (-)", value = 65, min = 0))
				)
			)
		),
		
		mainPanel(
			width = 8,
			tableOutput("tabela_resultados"),
			br(),
			plotOutput("forest_plot", height = "420px")
		)
	)
)

server <- function(input, output) {
	
	calcular_or <- function(nome, a, b, c, d) {
		if (any(c(a, b, c, d) == 0, na.rm = TRUE)) {
			a <- a + 0.5
			b <- b + 0.5
			c <- c + 0.5
			d <- d + 0.5
		}
		
		or <- (a * d) / (b * c)
		se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
		z <- qnorm(0.975)
		
		ic_inf <- exp(log(or) - z * se_log_or)
		ic_sup <- exp(log(or) + z * se_log_or)
		
		data.frame(
			Fator = nome,
			OR = or,
			IC_inf = ic_inf,
			IC_sup = ic_sup,
			stringsAsFactors = FALSE
		)
	}
	
	dados_or <- reactive({
		req(
			input$a1, input$b1, input$c1, input$d1,
			input$a2, input$b2, input$c2, input$d2,
			input$a3, input$b3, input$c3, input$d3
		)
		
		rbind(
			calcular_or(input$nome1, input$a1, input$b1, input$c1, input$d1),
			calcular_or(input$nome2, input$a2, input$b2, input$c2, input$d2),
			calcular_or(input$nome3, input$a3, input$b3, input$c3, input$d3)
		)
	})
	
	output$tabela_resultados <- renderTable({
		df <- dados_or()
		data.frame(
			`Fator de Risco` = df$Fator,
			`Odds Ratio` = sprintf("%.2f", df$OR),
			`IC 95% Inferior` = sprintf("%.2f", df$IC_inf),
			`IC 95% Superior` = sprintf("%.2f", df$IC_sup),
			`OR (IC 95%)` = sprintf("%.2f (%.2f - %.2f)", df$OR, df$IC_inf, df$IC_sup),
			check.names = FALSE
		)
	}, striped = TRUE, bordered = TRUE, hover = TRUE, align = "lcccc")
	
	output$forest_plot <- renderPlot({
		df <- dados_or()
		df$Fator_ordem <- factor(
			seq_len(nrow(df)),
			levels = rev(seq_len(nrow(df))),
			labels = rev(df$Fator)
		)
		df$Label <- sprintf("%.2f (%.2f - %.2f)", df$OR, df$IC_inf, df$IC_sup)
		
		ggplot(df, aes(x = OR, y = Fator_ordem)) +
			geom_vline(xintercept = 1, linetype = "dashed", color = "#C0392B", linewidth = 0.8) +
			geom_errorbar(aes(xmin = IC_inf, xmax = IC_sup), width = 0.15, linewidth = 0.8, color = "#2C3E50") +
			geom_point(size = 3.5, color = "#1F618D") +
			geom_text(aes(label = Label), vjust = -1.2, size = 3.8, color = "#2C3E50") +
			scale_x_log10() +
			labs(
				title = "Forest Plot - Odds Ratio (IC 95%)",
				x = "Odds Ratio (escala logarítmica)",
				y = NULL
			) +
			theme_minimal(base_size = 13) +
			theme(
				panel.grid.minor = element_blank(),
				axis.text.y = element_text(face = "bold", size = 12),
				plot.title = element_text(face = "bold")
			)
	})
}

shinyApp(ui = ui, server = server)

