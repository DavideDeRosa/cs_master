// Example_1.cpp : Questo file contiene la funzione 'main', in cui inizia e termina l'esecuzione
// del programma. Apre una finestra e gestisce qualche evento, mouse, tastiera.
//
#include <glad/glad.h> //be sure to include GLAD before other header files that require OpenGL (like GLFW).
#include <GLFW/glfw3.h>
#include <iostream>

//Prototipi delle callback function agli eventi
void mouse_button_callback(GLFWwindow* window, int button, int action, int mods);
void cursor_position_callback(GLFWwindow* window, double xpos, double ypos);
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods);
void framebuffer_size_callback(GLFWwindow* window, int width, int height);

double  xpos, ypos;
float   r = 0.0, g = 1.0, b = 0.0;  //Per impostare il colore inizale del background a verde

// Function to initialize OpenGL and create a window.
GLFWwindow* initOpenGL() {
    int  height = 800, width = 800;

    // Initialize GLFW.
    /******* Inizializzazioni GLFW library *************/
    if (!glfwInit()) {
        std::cout << "Failed to initialize GLFW." << std::endl;
        glfwTerminate();
    }

    //Imposta le propriet� del contesto e del profile
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    //Abilita il double buffering
    glfwWindowHint(GLFW_DOUBLEBUFFER, GLFW_TRUE);

    /******* Create a window and its OpenGL context *****/
    GLFWwindow* window = glfwCreateWindow(width, height, "Hello World", NULL, NULL);
    if (!window)
    {
        std::cout << "Failed to create GLFW window !" << std::endl;
        glfwTerminate();
    }
    /* Make the window's context current */
    glfwMakeContextCurrent(window); //crea il context corrente e lo associa a window. In opengl un rendering context � una macchina astati che memorizza tutte le informazioni necessarie e le risorse per il rendering grafico

    // verifica se la libreria GLAD � riuscita a caricare correttamente tutti i puntatori
    // alle funzioni OpenGL necessarie.
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        std::cout << "Failed to load opengl function pointers !" << std::endl;
        glfwTerminate();
    }
    return window;
}

int main(void)
{
    // Initialize OpenGL and create a window.
    GLFWwindow* window = initOpenGL();

    // Definizione delle funzioni di callback che verranno chiamate quando si verificano
    // determinati eventi

    // Chiudi l'applicazione premendo il tasto Esc
    // Modifica il colore dello sfondo con il tasto space bar
    glfwSetKeyCallback(window, key_callback);
    //Visualizza le coordinate del mouse che si muove sulla finestra grafica
    glfwSetCursorPosCallback(window, cursor_position_callback);
    //Visualizza le coordinate individuate dal tasto sinistro premuto
    glfwSetMouseButtonCallback(window, mouse_button_callback);
    //Visualizza le dimensioni della finestra ridimensionata
    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

    /* Loop until the user closes the window */
    while (!glfwWindowShouldClose(window))
    {
        glClearColor(r, g, b, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT);

        /* Rendering */
        /* ... */

        /* Swap front and back buffers */
        glfwSwapBuffers(window);
        /* Poll for and process events */
        glfwPollEvents();
    }

    glfwTerminate();
    return 0;
}

//Callback functions
void cursor_position_callback(GLFWwindow* window, double xpos, double ypos) {

    // Visualizza le coordinate del mouse che si muove sulla finestra grafica
    std::cout << "Px: " << xpos << " y: " << ypos << std::endl;
}
void mouse_button_callback(GLFWwindow* window, int button, int action, int mods)
{
    // Se il bottone sinistro del mouse �  premuto, visualizza le coordinate
    // del mouse nella posizione in cui il bottone centrale �  premuto.
    if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS)
    {
        glfwGetCursorPos(window, &xpos, &ypos);
        std::cout << "Coordinate del mouse tasto sinistro premuto : x = " << xpos << ", y = " << ypos << std::endl;
    }
}
void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods) {

    // Se il tasto ESCAPE �  premuto, chiude la finestra
    switch (key) {
    case GLFW_KEY_ESCAPE:
        if (action == GLFW_PRESS)
            //Imposta a True il flag booleano di chiusura della finestra
            glfwSetWindowShouldClose(window, GLFW_TRUE);
        break;

    case GLFW_KEY_SPACE:
        if (action == GLFW_PRESS) {
            r = 1.0;
            g = 0.0;
            b = 0.0;
        }
        //Quando il testo SPACE viene rilasciato lo sfondo riassume colore verde
        else if (action == GLFW_RELEASE) {
            r = 0.0;
            g = 1.0;
            b = 0.0;
        }
        break;

    default:

        break;
    }
}
void framebuffer_size_callback(GLFWwindow* window, int width, int height) {
    //  Stampa le dimensioni della finestra ridimensionata
    std::cout << "Window size: width: " << width << " height " << height << std::endl;
}